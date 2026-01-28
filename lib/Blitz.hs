module Blitz where

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

tileW, tileH, tilesX, tilesY, numTiles :: Int
tileW = 16
tileH = 16
tilesX = (fbW + tileW - 1) `Prelude.div` tileW
tilesY = (fbH + tileH - 1) `Prelude.div` tileH
numTiles = tilesX * tilesY

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

distToSegmentSq :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
distToSegmentSq px py x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
      vx = px - x1
      vy = py - y1
   in A.cond
        (l2 A.== 0)
        (vx * vx + vy * vy)
        ( let t = A.max 0 (A.min 1 ((vx * dx + vy * dy) / l2))
              projX = x1 + t * dx
              projY = y1 + t * dy
              dxp = px - projX
              dyp = py - projY
           in dxp * dxp + dyp * dyp
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

      -- Precompute bounding boxes
      primSh = A.index1 (A.size tags)
      isCircleAt i = tags A.!! i A.== A.constant circleTagVal
      calc i = (x1s A.!! i, y1s A.!! i, x2s A.!! i, y2s A.!! i, ss A.!! i)
      minXs = A.generate primSh $ \ix -> let i = A.unindex1 ix; (x1, _, x2, _, s) = calc i in isCircleAt i ? (x1 - s, A.min x1 x2 - s)
      maxXs = A.generate primSh $ \ix -> let i = A.unindex1 ix; (x1, _, x2, _, s) = calc i in isCircleAt i ? (x1 + s, A.max x1 x2 + s)
      minYs = A.generate primSh $ \ix -> let i = A.unindex1 ix; (_, y1, _, y2, s) = calc i in isCircleAt i ? (y1 - s, A.min y1 y2 - s)
      maxYs = A.generate primSh $ \ix -> let i = A.unindex1 ix; (_, y1, _, y2, s) = calc i in isCircleAt i ? (y1 + s, A.max y1 y2 + s)

      -- Overlap matrix as Int: (numTiles, numPrims) — 1 if prim's bbox overlaps tile, 0 otherwise
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

      -- Source array: each (tile, prim) position holds prim index
      primIndices =
        A.generate (A.constant (Z :. numTiles :. numPrims)) $ \ix ->
          let Z :. _ :. p = A.unlift ix :: Z :. Exp Int :. Exp Int
           in p

      -- Build compact tile bins via permute (no loops!)
      -- Scatter prim indices to their compact positions
      defaultBins = A.fill (A.constant (Z :. numTiles :. numPrims)) (0 :: Exp Int)
      tileBins =
        A.permute
          const -- combination (no collisions expected)
          defaultBins
          ( \ix ->
              let Z :. t :. _ = A.unlift ix :: Z :. Exp Int :. Exp Int
                  overlap = overlapInt A.! ix
                  slot = prefixSum A.! ix
               in A.cond
                    (overlap A.== 1)
                    (A.Just_ (A.lift (Z :. t :. slot)))
                    A.Nothing_
          )
          primIndices
   in -- Pixel shader: iterate only the compact bin for this tile
      A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = A.unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x :: Exp Float
            py = A.fromIntegral y :: Exp Float
            tileId = (y `A.quot` A.constant tileH) * A.constant tilesX + (x `A.quot` A.constant tileW)
            tileCount = tileCounts A.! A.index1 tileId
            initial = A.lift (tileCount - 1, A.constant 0xFF000000 :: Exp Word32)
            wcond st = let (i, _) = A.unlift st :: (Exp Int, Exp Word32) in i A.>= 0
            body st =
              let (i, acc) = A.unlift st :: (Exp Int, Exp Word32)
                  primIdx = tileBins A.! A.lift (Z :. tileId :. i)
                  tag = tags A.!! primIdx
                  lx1 = x1s A.!! primIdx
                  ly1 = y1s A.!! primIdx
                  lx2 = x2s A.!! primIdx
                  ly2 = y2s A.!! primIdx
                  s = ss A.!! primIdx
                  col = cols A.!! primIdx
                  inBox =
                    px A.>= minXs A.!! primIdx
                      A.&& px A.<= maxXs A.!! primIdx
                      A.&& py A.>= minYs A.!! primIdx
                      A.&& py A.<= maxYs A.!! primIdx
                  isCircle = tag A.== A.constant circleTagVal
                  ddx = px - lx1
                  ddy = py - ly1
                  circleHit = ddx * ddx + ddy * ddy A.< s * s
                  lineHit = distToSegmentSq px py lx1 ly1 lx2 ly2 A.< s * s
                  isHit = inBox A.&& (isCircle ? (circleHit, lineHit))
                  newAcc = isHit ? (col, acc)
                  newI = isHit ? (A.constant (-1), i - 1)
               in A.lift (newI, newAcc)
         in A.snd (A.while wcond body initial)

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
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
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  let update i
        | i Prelude.== numPrims = return ()
        | otherwise = do
            let fi = Prelude.fromIntegral (i + 1)
                pxAt = 160 + 140 * cos (frame / 30 + fi * 0.1)
                pyAt = 100 + 80 * sin (frame / 50 + fi * 0.2)
                isCircle = (i + 1) `Prelude.rem` 2 Prelude.== 0
                colAt = 0xFF000000 + Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` 0x00FFFFFF)

            VSM.unsafeWrite env.mTags i (if isCircle then circleTagVal else lineTagVal)
            VSM.unsafeWrite env.mX1s i (if isCircle then pxAt else 160)
            VSM.unsafeWrite env.mY1s i (if isCircle then pyAt else 100)
            VSM.unsafeWrite env.mX2s i (if isCircle then 0 else pxAt)
            VSM.unsafeWrite env.mY2s i (if isCircle then 0 else pyAt)
            VSM.unsafeWrite env.mSizes i (if isCircle then 5 + 3 * sin (frame / 10 + fi) else 1)
            VSM.unsafeWrite env.mColors i colAt
            update (i + 1)
  update 0

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
