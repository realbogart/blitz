module Blitz where

import Control.Concurrent (forkOS)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits qualified as ABits
-- import Data.Array.Accelerate.LLVM.Native as CPU
import Data.Array.Accelerate.Interpreter as CPU
import Data.Bits
import Data.IORef
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util.Colors

type TickAction = IO ()

windowWidth, windowHeight :: Int
windowWidth = 1920
windowHeight = 1200

targetFramesPerSecond :: Int
targetFramesPerSecond = 60

fbW, fbH :: Int
fbW = 320
fbH = 180

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
rgba r g b a =
  (Prelude.fromIntegral r `shiftL` 0)
    .|. (Prelude.fromIntegral g `shiftL` 8)
    .|. (Prelude.fromIntegral b `shiftL` 16)
    .|. (Prelude.fromIntegral a `shiftL` 24)

-- Accelerate kernel: given frame number, generate fbH x fbW packed RGBA pixels
renderAcc :: Acc (Scalar Int) -> Acc (Array DIM2 Word32)
renderAcc t =
  A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
    let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
        f = the t
        r = A.fromIntegral ((x + f) `A.rem` 256) :: Exp Word32
        g = A.fromIntegral ((y + f) `A.rem` 256) :: Exp Word32
        b = A.fromIntegral ((x + y) `A.rem` 256) :: Exp Word32
        a = 255 :: Exp Word32
     in (r `ABits.shiftL` 0)
          ABits..|. (g `ABits.shiftL` 8)
          ABits..|. (b `ABits.shiftL` 16)
          ABits..|. (a `ABits.shiftL` 24)

-- Minimal upload path: copy [Word32] into your pixel pointer
copyPixels :: Ptr Word32 -> [Word32] -> IO ()
copyPixels p = go 0
  where
    go _ [] = pure ()
    go i (w : ws) = pokeByteOff p (i * 4) w >> go (i + 1) ws

{-# NOINLINE tickStore #-}
tickStore :: FS.Store (IORef TickAction)
tickStore = FS.Store 0

main :: IO ()
main = do
  tickRef <- newIORef (pure ())
  runWindow tickRef

mainDev :: IO ()
mainDev = do
  let FS.Store storeId = tickStore
  existing <- FS.lookupStore storeId
  case existing of
    Nothing -> do
      putStrLn "booting"
      tickRef <- newIORef (pure ())
      FS.writeStore tickStore tickRef
      void $ forkOS $ do
        runWindow tickRef
        FS.deleteStore tickStore
        putStrLn "shutting down"
    Just _ -> do
      putStrLn "reloading"
      tickRef <- FS.readStore tickStore
      atomicWriteIORef tickRef (pure ())

runWindow :: IORef TickAction -> IO ()
runWindow tickRef = do
  window <- initWindow windowWidth windowHeight "blitz"
  setTargetFPS targetFramesPerSecond

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  pixels <- mallocArray (fbW * fbH) :: IO (Ptr Word32)
  frameRef <- newIORef (0 :: Int)

  -- Compile once (important even for “minimal”)
  let render1 = CPU.run1 renderAcc

  atomicWriteIORef tickRef $ do
    frame <- readIORef frameRef
    modifyIORef' frameRef (+ 1)

    -- Accelerate render (CPU backend)
    let arr :: Array DIM2 Word32
        arr = render1 (A.fromList Z [frame] :: Scalar Int)

    -- Minimal (slow) copy out
    copyPixels pixels (A.toList arr)

    -- Upload + present
    updateTexture tex (castPtr pixels)

    sw <- getScreenWidth
    sh <- getScreenHeight
    beginDrawing
    clearBackground black
    let src = Rectangle 0 0 (Prelude.fromIntegral fbW) (Prelude.fromIntegral fbH)
        dst = Rectangle 0 0 (Prelude.fromIntegral sw) (Prelude.fromIntegral sh)
        origin = Vector2 0 0
    drawTexturePro tex src dst origin 0 white
    endDrawing

  gameLoop tickRef

  unloadTexture tex window
  closeWindow (Just window)

gameLoop :: IORef TickAction -> IO ()
gameLoop tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn
    gameLoop tickRef
