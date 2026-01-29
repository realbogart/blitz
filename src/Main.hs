module Main where

import Blitz qualified
import Blitz.Draw
import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad (unless, void, when)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.IORef
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Ptr (castPtr)
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Text (drawFPS)
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

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envPausedRef :: IORef Bool,
    envRender :: Blitz.Inputs -> Array DIM2 Word32,
    envInputs :: Blitz.Inputs,
    vTags :: VS.Vector Int32,
    vX1s :: VS.Vector Float,
    vY1s :: VS.Vector Float,
    vX2s :: VS.Vector Float,
    vY2s :: VS.Vector Float,
    vSizes :: VS.Vector Float,
    mTags :: VSM.IOVector Int32,
    mX1s :: VSM.IOVector Float,
    mY1s :: VSM.IOVector Float,
    mX2s :: VSM.IOVector Float,
    mY2s :: VSM.IOVector Float,
    mSizes :: VSM.IOVector Float,
    mColors :: VSM.IOVector Word32,
    mTileCounts :: VSM.IOVector Int32,
    mTileBins :: VSM.IOVector Int32
  }

tick :: Tick
tick env = do
  -- Toggle pause with 'P' key
  pPressed <- isKeyPressed KeyP
  when pPressed $ modifyIORef' env.envPausedRef Prelude.not

  paused <- readIORef env.envPausedRef
  f <- readIORef env.envFrameRef
  unless paused $ modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  -- Scattered small primitives instead of large star burst (skip when paused)
  nPrimsDrawn <-
    if paused
      then pure Blitz.numPrims
      else
        runDrawFrame
          env.mTags
          env.mX1s
          env.mY1s
          env.mX2s
          env.mY2s
          env.mSizes
          env.mColors
          Blitz.numPrims
          Blitz.circleTagVal
          Blitz.lineTagVal
          (drawScene frame)

  Blitz.buildTileBins nPrimsDrawn env.vTags env.vX1s env.vY1s env.vX2s env.vY2s env.vSizes env.mTileCounts env.mTileBins

  let arr = env.envRender env.envInputs
  let vec = AVS.toVectors arr

  VS.unsafeWith vec $ \srcPtr -> updateTexture env.envTex (castPtr srcPtr)

  beginDrawing
  clearBackground black
  let src = Rectangle 0 0 (Prelude.fromIntegral Blitz.fbW) (Prelude.fromIntegral Blitz.fbH)
      dst = Rectangle 0 0 (Prelude.fromIntegral windowWidth) (Prelude.fromIntegral windowHeight)
  drawTexturePro env.envTex src dst (Vector2 0 0) 0 white
  drawFPS 10 10
  endDrawing

drawScene :: Float -> DrawM ()
drawScene frame = go 0
  where
    go !i
      | i Prelude.== Blitz.numPrims = pure ()
      | otherwise = do
          let fi = Prelude.fromIntegral (i + 1) :: Float
              -- Pseudo-random but deterministic positions using the index
              baseX = Prelude.fromIntegral ((i * 97) `Prelude.rem` Blitz.fbW)
              baseY = Prelude.fromIntegral ((i * 61) `Prelude.rem` Blitz.fbH)
              -- Small oscillation around base position
              pxAt = baseX + 10 * cos (frame / 40 + fi * 0.3)
              pyAt = baseY + 10 * sin (frame / 35 + fi * 0.4)
              isCircle = (i + 1) `Prelude.rem` 3 Prelude./= 0 -- 2/3 circles, 1/3 lines
              colAt = 0xFF000000 + Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` 0x00FFFFFF)
          if isCircle
            then drawCircle pxAt pyAt (3 + 2 * sin (frame / 15 + fi)) colAt
            else do
              -- Short lines: 15-25 pixels long
              let lineLen = 15 + 10 * sin (fi * 0.5)
                  lineAngle = frame / 60 + fi * 0.7
                  lx2 = pxAt + lineLen * cos lineAngle
                  ly2 = pyAt + lineLen * sin lineAngle
              drawLine pxAt pyAt lx2 ly2 1 colAt
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

  -- Pre-allocate mutable vectors
  tagsV <- VSM.new Blitz.numPrims
  x1sV <- VSM.new Blitz.numPrims
  y1sV <- VSM.new Blitz.numPrims
  x2sV <- VSM.new Blitz.numPrims
  y2sV <- VSM.new Blitz.numPrims
  sizesV <- VSM.new Blitz.numPrims
  colorsV <- VSM.new Blitz.numPrims
  tileCountsV <- VSM.new Blitz.numTiles
  tileBinsV <- VSM.new (Blitz.numTiles * Blitz.maxPrimsPerTile)

  -- Reuse frozen views to avoid per-frame input allocations; data updates via the mutable vectors.
  shTags <- VS.unsafeFreeze tagsV
  shX1s <- VS.unsafeFreeze x1sV
  shY1s <- VS.unsafeFreeze y1sV
  shX2s <- VS.unsafeFreeze x2sV
  shY2s <- VS.unsafeFreeze y2sV
  shSizes <- VS.unsafeFreeze sizesV
  shColors <- VS.unsafeFreeze colorsV
  shTileCounts <- VS.unsafeFreeze tileCountsV
  shTileBins <- VS.unsafeFreeze tileBinsV

  let sh = Z :. Blitz.numPrims
      shTileCountsShape = Z :. Blitz.numTiles
      shTileBinsShape = Z :. (Blitz.numTiles * Blitz.maxPrimsPerTile)
      inputs =
        ( AVS.fromVectors sh shTags,
          AVS.fromVectors sh shX1s,
          AVS.fromVectors sh shY1s,
          AVS.fromVectors sh shX2s,
          AVS.fromVectors sh shY2s,
          AVS.fromVectors sh shSizes,
          AVS.fromVectors sh shColors,
          AVS.fromVectors shTileCountsShape shTileCounts,
          AVS.fromVectors shTileBinsShape shTileBins
        )

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envPausedRef = pausedRef,
            envRender = CPU.run1 Blitz.renderPipeline,
            envInputs = inputs,
            vTags = shTags,
            vX1s = shX1s,
            vY1s = shY1s,
            vX2s = shX2s,
            vY2s = shY2s,
            vSizes = shSizes,
            mTags = tagsV,
            mX1s = x1sV,
            mY1s = y1sV,
            mX2s = x2sV,
            mY2s = y2sV,
            mSizes = sizesV,
            mColors = colorsV,
            mTileCounts = tileCountsV,
            mTileBins = tileBinsV
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
