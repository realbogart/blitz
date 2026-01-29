module Main where

import Blitz qualified
import Blitz.Draw
import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.IORef
import Foreign.Ptr (castPtr)
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures
import Raylib.Types hiding (Camera2D)
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

demoPrims :: Int
demoPrims = min 1000 Blitz.maxNumPrims

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envPausedRef :: IORef Bool,
    envResources :: Blitz.Resources
  }

tick :: Tick
tick env = do
  pPressed <- isKeyPressed KeyP
  when pPressed $ modifyIORef' env.envPausedRef Prelude.not

  paused <- readIORef env.envPausedRef
  f <- readIORef env.envFrameRef
  unless paused $ modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  if paused
    then Blitz.withFramePixelsCached env.envResources $ \srcPtr ->
      updateTexture env.envTex (castPtr srcPtr)
    else Blitz.withFramePixels env.envResources (drawScene frame) $ \srcPtr ->
      updateTexture env.envTex (castPtr srcPtr)

  beginDrawing
  clearBackground black
  let src = Rectangle 0 0 (Prelude.fromIntegral Blitz.fbW) (Prelude.fromIntegral Blitz.fbH)
      dst = Rectangle 0 0 (Prelude.fromIntegral windowWidth) (Prelude.fromIntegral windowHeight)
  drawTexturePro env.envTex src dst (Vector2 0 0) 0 white
  drawFPS 10 10
  endDrawing

drawScene :: Float -> DrawM ()
drawScene frame = do
  let t = frame / 60
      camZoomAt = Prelude.max 0.05 (1.4 + 4.6 * sin (t * 0.5))
      cam =
        Camera2D
          { camCenterX = sin t * 5.5,
            camCenterY = cos t * 4.5,
            camZoom = camZoomAt
          }
  withCamera cam $ do
    drawLine (-4) 0 4 0 0.02 0xFF2A2A2A
    drawLine 0 (-3) 0 3 0.02 0xFF2A2A2A
    go 0
  drawLine 12 12 160 12 2 0xFF00D06A
  drawCircle 24 28 6 0xFFFFC857
  where
    go !i
      | i Prelude.== demoPrims = pure ()
      | otherwise = do
          let fi = Prelude.fromIntegral (i + 1) :: Float
              baseX = Prelude.fromIntegral ((i * 97) `Prelude.rem` 160) / 20 - 4.0
              baseY = Prelude.fromIntegral ((i * 61) `Prelude.rem` 120) / 15 - 4.0
              pxAt = baseX + 0.8 * cos (frame / 40 + fi * 0.3)
              pyAt = baseY + 0.8 * sin (frame / 35 + fi * 0.4)
              isCircle = (i + 1) `Prelude.rem` 3 Prelude./= 0 -- 2/3 circles, 1/3 lines
              colAt = 0xFF000000 + Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` 0x00FFFFFF)
          if isCircle
            then drawCircle pxAt pyAt (0.12 + 0.06 * sin (frame / 15 + fi)) colAt
            else do
              let lineLen = 0.45 + 0.2 * sin (fi * 0.5)
                  lineAngle = frame / 60 + fi * 0.7
                  lx2 = pxAt + lineLen * cos lineAngle
                  ly2 = pyAt + lineLen * sin lineAngle
              drawLine pxAt pyAt lx2 ly2 0.06 colAt
          go (i + 1)

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

  img <- genImageColor Blitz.fbW Blitz.fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  frameRef <- newIORef (0 :: Int)
  pausedRef <- newIORef False

  resources <- Blitz.initResources

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envPausedRef = pausedRef,
            envResources = resources
          }

  gameLoop env tickRef `finally` do
    unloadTexture tex window
    closeWindow (Just window)

gameLoop :: Env -> IORef Tick -> IO ()
gameLoop env tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn env
    gameLoop env tickRef
