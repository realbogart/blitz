module Blitz where

import Control.Concurrent (forkOS)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits qualified as ABits
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.IORef
import Data.Vector.Storable qualified as VS
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors

type Tick = Env -> IO ()

{-# NOINLINE tickStore #-}
tickStore :: FS.Store (IORef Tick)
tickStore = FS.Store 0

windowWidth, windowHeight :: Int
windowWidth = 1920
windowHeight = 1200

targetFramesPerSecond :: Int
targetFramesPerSecond = 60

fbW, fbH :: Int
fbW = 320
fbH = 180

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

copyPixels :: Ptr Word32 -> [Word32] -> IO ()
copyPixels p = go 0
  where
    go _ [] = pure ()
    go i (w : ws) = pokeByteOff p (i * 4) w >> go (i + 1) ws

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envPixels :: Ptr Word32,
    envFrameRef :: IORef Int,
    envRender :: Scalar Int -> Array DIM2 Word32
  }

tick :: Tick
tick env = do
  frame <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)

  let arr = env.envRender (A.fromList Z [frame] :: Scalar Int)
  let vec :: VS.Vector Word32
      vec = AVS.toVectors arr

  VS.unsafeWith vec $ \srcPtr ->
    updateTexture env.envTex (castPtr srcPtr)

  sw <- getScreenWidth
  sh <- getScreenHeight

  beginDrawing
  clearBackground black
  let src = Rectangle 0 0 (Prelude.fromIntegral fbW) (Prelude.fromIntegral fbH)
      dst = Rectangle 0 0 (Prelude.fromIntegral sw) (Prelude.fromIntegral sh)
      origin = Vector2 0 0
  drawTexturePro env.envTex src dst origin 0 white
  endDrawing

main :: IO ()
main = do
  tickRef <- newIORef tick
  runWindow tickRef

mainDev :: IO ()
mainDev = do
  let FS.Store storeId = tickStore
  existing <- FS.lookupStore storeId
  case existing of
    Nothing -> do
      putStrLn "booting"
      tickRef <- newIORef tick
      FS.writeStore tickStore tickRef
      void $ forkOS $ do
        runWindow tickRef
        FS.deleteStore tickStore
        putStrLn "shutting down"
    Just _ -> do
      putStrLn "reloading"
      tickRef <- FS.readStore tickStore
      atomicWriteIORef tickRef tick

runWindow :: IORef Tick -> IO ()
runWindow tickRef = do
  window <- initWindow windowWidth windowHeight "blitz"
  setTargetFPS targetFramesPerSecond

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  pixels <- mallocArray (fbW * fbH) :: IO (Ptr Word32)
  frameRef <- newIORef (0 :: Int)

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envPixels = pixels,
            envFrameRef = frameRef,
            envRender = (GPU.run1 renderAcc)
          }

  gameLoop env tickRef

  unloadTexture tex window
  closeWindow (Just window)

gameLoop :: Env -> IORef Tick -> IO ()
gameLoop env tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn env
    gameLoop env tickRef
