module Blitz where

import Blitz.Draw
import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
-- import Data.Array.Accelerate.LLVM.PTX as GPU
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

fbW, fbH :: Int
fbW = 320
fbH = 200

numPrims :: Int
numPrims = 2000

tileW, tileH, tilesX, tilesY, numTiles, maxPrimsPerTile :: Int
tileW = 16
tileH = 16
tilesX = (fbW + tileW - 1) `Prelude.div` tileW
tilesY = (fbH + tileH - 1) `Prelude.div` tileH
numTiles = tilesX * tilesY
maxPrimsPerTile = 128 -- Cap bin size to reduce memory footprint

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

-- Optimized line hit test: returns True if pixel is within threshold of line segment
-- Avoids division by multiplying through
{-# INLINE lineHitTest #-}
lineHitTest :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Bool
lineHitTest px py x1 y1 x2 y2 threshold =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
      vx = px - x1
      vy = py - y1
      v2 = vx * vx + vy * vy
      u = vx * dx + vy * dy -- projection of v onto d
      wx = px - x2
      wy = py - y2
      w2 = wx * wx + wy * wy
      threshSq = threshold * threshold
   in A.cond
        (l2 A.== 0)
        (v2 A.< threshSq) -- degenerate: point
        ( A.cond
            (u A.<= 0)
            (v2 A.< threshSq) -- closest to p1
            ( A.cond
                (u A.>= l2)
                (w2 A.< threshSq) -- closest to p2
                (v2 * l2 - u * u A.< threshSq * l2) -- closest to segment interior (no division!)
            )
        )

type Inputs = (Vector Int32, Vector Float, Vector Float, Vector Float, Vector Float, Vector Float, Vector Word32)

renderPipeline :: Acc Inputs -> Acc (Array DIM2 Word32)
renderPipeline input =
  let -- Unpack inputs
      (tags, x1s, y1s, x2s, y2s, ss, cols) =
        ( A.unlift input ::
            ( Acc (Vector Int32),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32)
            )
        )

      -- Precompute bounding boxes as arrays (enables better memory access patterns)
      primSh = A.index1 (A.size tags)
      isCircleAt i = tags A.!! i A.== A.constant circleTagVal
      minXs = A.generate primSh $ \ix ->
        let i = A.unindex1 ix
            x1 = x1s A.!! i
            x2 = x2s A.!! i
            s = ss A.!! i
         in isCircleAt i ? (x1 - s, A.min x1 x2 - s)
      maxXs = A.generate primSh $ \ix ->
        let i = A.unindex1 ix
            x1 = x1s A.!! i
            x2 = x2s A.!! i
            s = ss A.!! i
         in isCircleAt i ? (x1 + s, A.max x1 x2 + s)
      minYs = A.generate primSh $ \ix ->
        let i = A.unindex1 ix
            y1 = y1s A.!! i
            y2 = y2s A.!! i
            s = ss A.!! i
         in isCircleAt i ? (y1 - s, A.min y1 y2 - s)
      maxYs = A.generate primSh $ \ix ->
        let i = A.unindex1 ix
            y1 = y1s A.!! i
            y2 = y2s A.!! i
            s = ss A.!! i
         in isCircleAt i ? (y1 + s, A.max y1 y2 + s)

      -- Overlap matrix: (numTiles, numPrims) — 1 if prim's bbox overlaps tile, 0 otherwise
      overlapInt =
        A.generate (A.constant (Z :. numTiles :. numPrims)) $ \ix ->
          let Z :. t :. p = A.unlift ix :: Z :. Exp Int :. Exp Int
              tx = t `A.rem` A.constant tilesX
              ty = t `A.quot` A.constant tilesX
              tMinX = A.fromIntegral (tx * A.constant tileW) :: Exp Float
              tMaxX = A.fromIntegral ((tx + 1) * A.constant tileW - 1) :: Exp Float
              tMinY = A.fromIntegral (ty * A.constant tileH) :: Exp Float
              tMaxY = A.fromIntegral ((ty + 1) * A.constant tileH - 1) :: Exp Float
              overlaps =
                maxXs A.!! p A.>= tMinX
                  A.&& minXs A.!! p A.<= tMaxX
                  A.&& maxYs A.!! p A.>= tMinY
                  A.&& minYs A.!! p A.<= tMaxY
           in A.cond overlaps (1 :: Exp Int) 0

      -- Exclusive prefix sum: gives slot positions; fold gives tile counts
      T2 prefixSum tileCounts = A.scanl' (+) 0 overlapInt

      -- Capped tile counts for bounded iteration
      cappedTileCounts = A.map (A.min (A.constant maxPrimsPerTile)) tileCounts

      -- Build compact tile bins via permute - CAPPED to maxPrimsPerTile
      defaultBins = A.fill (A.constant (Z :. numTiles :. maxPrimsPerTile)) (0 :: Exp Int)
      tileBins =
        A.permute
          const
          defaultBins
          ( \ix ->
              let Z :. t :. _ = A.unlift ix :: Z :. Exp Int :. Exp Int
                  overlap = overlapInt A.! ix
                  slot = prefixSum A.! ix
               in A.cond
                    (overlap A.== 1 A.&& slot A.< A.constant maxPrimsPerTile)
                    (A.Just_ (A.lift (Z :. t :. slot)))
                    A.Nothing_
          )
          ( A.generate (A.constant (Z :. numTiles :. numPrims)) $ \ix ->
              let Z :. _ :. primIdx = A.unlift ix :: Z :. Exp Int :. Exp Int
               in primIdx
          )
   in -- Pixel shader: iterate only the compact bin for this tile
      A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = A.unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x :: Exp Float
            py = A.fromIntegral y :: Exp Float
            tileId = (y `A.quot` A.constant tileH) * A.constant tilesX + (x `A.quot` A.constant tileW)
            tileCount = cappedTileCounts A.! A.index1 tileId
            initial = A.lift (tileCount - 1, A.constant 0xFF000000 :: Exp Word32)
            wcond st = let (i, _) = A.unlift st :: (Exp Int, Exp Word32) in i A.>= 0
            body st =
              let (i, acc) = A.unlift st :: (Exp Int, Exp Word32)
                  primIdx = tileBins A.! A.lift (Z :. tileId :. i)
                  -- Quick bbox rejection first (cheap)
                  inBox =
                    px A.>= minXs A.!! primIdx
                      A.&& px A.<= maxXs A.!! primIdx
                      A.&& py A.>= minYs A.!! primIdx
                      A.&& py A.<= maxYs A.!! primIdx
               in A.cond
                    (A.not inBox)
                    (A.lift (i - 1, acc)) -- skip if outside bbox
                    ( let tag = tags A.!! primIdx
                          lx1 = x1s A.!! primIdx
                          ly1 = y1s A.!! primIdx
                          lx2 = x2s A.!! primIdx
                          ly2 = y2s A.!! primIdx
                          s = ss A.!! primIdx
                          col = cols A.!! primIdx
                          isCircle = tag A.== A.constant circleTagVal
                          ddx = px - lx1
                          ddy = py - ly1
                          circleHit = ddx * ddx + ddy * ddy A.< s * s
                          lineHit = lineHitTest px py lx1 ly1 lx2 ly2 s
                          isHit = isCircle ? (circleHit, lineHit)
                          newAcc = isHit ? (col, acc)
                          newI = isHit ? (A.constant (-1), i - 1)
                       in A.lift (newI, newAcc)
                    )
         in A.snd (A.while wcond body initial)

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envPausedRef :: IORef Bool,
    envRender :: Inputs -> Array DIM2 Word32,
    mTags :: VSM.IOVector Int32,
    mX1s :: VSM.IOVector Float,
    mY1s :: VSM.IOVector Float,
    mX2s :: VSM.IOVector Float,
    mY2s :: VSM.IOVector Float,
    mSizes :: VSM.IOVector Float,
    mColors :: VSM.IOVector Word32
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
  unless paused $
    void $
      runDrawFrame
        env.mTags
        env.mX1s
        env.mY1s
        env.mX2s
        env.mY2s
        env.mSizes
        env.mColors
        numPrims
        circleTagVal
        lineTagVal
        (drawScene frame)

  shTags <- VS.unsafeFreeze env.mTags
  shX1s <- VS.unsafeFreeze env.mX1s
  shY1s <- VS.unsafeFreeze env.mY1s
  shX2s <- VS.unsafeFreeze env.mX2s
  shY2s <- VS.unsafeFreeze env.mY2s
  shSizes <- VS.unsafeFreeze env.mSizes
  shColors <- VS.unsafeFreeze env.mColors

  let sh = Z :. numPrims
      inputs =
        ( AVS.fromVectors sh shTags,
          AVS.fromVectors sh shX1s,
          AVS.fromVectors sh shY1s,
          AVS.fromVectors sh shX2s,
          AVS.fromVectors sh shY2s,
          AVS.fromVectors sh shSizes,
          AVS.fromVectors sh shColors
        )

  let arr = env.envRender inputs
  let vec = AVS.toVectors arr

  VS.unsafeWith vec $ \srcPtr -> updateTexture env.envTex (castPtr srcPtr)

  beginDrawing
  clearBackground black
  let src = Rectangle 0 0 (Prelude.fromIntegral fbW) (Prelude.fromIntegral fbH)
      dst = Rectangle 0 0 (Prelude.fromIntegral windowWidth) (Prelude.fromIntegral windowHeight)
  drawTexturePro env.envTex src dst (Vector2 0 0) 0 white
  drawFPS 10 10
  endDrawing

-- | Draw the scene with scattered small primitives
drawScene :: Float -> DrawM ()
drawScene frame = go 0
  where
    go !i
      | i Prelude.== numPrims = pure ()
      | otherwise = do
          let fi = Prelude.fromIntegral (i + 1) :: Float
              -- Pseudo-random but deterministic positions using the index
              baseX = Prelude.fromIntegral ((i * 97) `Prelude.rem` fbW)
              baseY = Prelude.fromIntegral ((i * 61) `Prelude.rem` fbH)
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

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  frameRef <- newIORef (0 :: Int)
  pausedRef <- newIORef False

  -- Pre-allocate mutable vectors
  tagsV <- VSM.new numPrims
  x1sV <- VSM.new numPrims
  y1sV <- VSM.new numPrims
  x2sV <- VSM.new numPrims
  y2sV <- VSM.new numPrims
  sizesV <- VSM.new numPrims
  colorsV <- VSM.new numPrims

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envPausedRef = pausedRef,
            envRender = CPU.run1 renderPipeline,
            mTags = tagsV,
            mX1s = x1sV,
            mY1s = y1sV,
            mX2s = x2sV,
            mY2s = y2sV,
            mSizes = sizesV,
            mColors = colorsV
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
