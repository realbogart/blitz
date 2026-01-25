module Blitz where

import Control.Concurrent (forkOS)
-- import Data.Array.Accelerate.LLVM.Native as CPU

import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits qualified as ABits
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.IORef
import Data.Vector.Storable qualified as VS
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (sizeOf)
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
targetFramesPerSecond = 120

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

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envPixels :: Ptr Word32,
    envFrameRef :: IORef Int,
    envRender :: Scalar Int -> Array DIM2 Word32,
    envNBytes :: Int
  }

tick :: Tick
tick env = do
  frame <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)

  let arr = env.envRender (A.fromFunction Z (const frame) :: Scalar Int)
  let vec :: VS.Vector Word32
      vec = AVS.toVectors arr

  VS.unsafeWith vec $ \srcPtr -> do
    copyBytes (castPtr env.envPixels) (castPtr srcPtr) env.envNBytes
    updateTexture env.envTex (castPtr env.envPixels)

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
  setConfigFlags [VsyncHint]
  window <- initWindow windowWidth windowHeight "blitz"
  setTargetFPS targetFramesPerSecond

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  pixels <- mallocArray (fbW * fbH) :: IO (Ptr Word32)
  frameRef <- newIORef (0 :: Int)

  let nbytes = fbW * fbH * sizeOf (undefined :: Word32)
  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envPixels = pixels,
            envFrameRef = frameRef,
            envRender = GPU.run1 renderAcc,
            envNBytes = nbytes
          }

  gameLoop env tickRef
    `finally` do
      unloadTexture tex window
      free pixels
      closeWindow (Just window)

gameLoop :: Env -> IORef Tick -> IO ()
gameLoop env tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn env
    gameLoop env tickRef
