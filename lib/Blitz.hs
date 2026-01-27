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

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

distToSegmentSq ::
  Exp Float ->
  Exp Float ->
  Exp Float ->
  Exp Float ->
  Exp Float ->
  Exp Float ->
  Exp Float
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

-- Primitive inputs: (tag, x1/cx, y1/cy, x2, y2, size/radius, color)
type Inputs =
  ( Vector Int32,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Word32
  )

-- With precomputed bounding boxes
type InputsWithBounds =
  ( Vector Int32,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Word32,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float
  )

-- | Precompute axis-aligned bounding boxes for all primitives.
precomputeBounds :: Acc Inputs -> Acc InputsWithBounds
precomputeBounds input =
  let (tags, x1s, y1s, x2s, y2s, ss, cols) =
        ( unlift input ::
            ( Acc (Vector Int32),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32)
            )
        )
      count = A.size tags
      sh = A.index1 count

      isCircle i = tags A.!! i A.== A.constant circleTagVal

      minXs =
        A.generate sh $ \ix ->
          let i = A.unindex1 ix
              x1 = x1s A.!! i
              x2 = x2s A.!! i
              s = ss A.!! i
           in isCircle i ? (x1 - s, A.min x1 x2 - s)

      maxXs =
        A.generate sh $ \ix ->
          let i = A.unindex1 ix
              x1 = x1s A.!! i
              x2 = x2s A.!! i
              s = ss A.!! i
           in isCircle i ? (x1 + s, A.max x1 x2 + s)

      minYs =
        A.generate sh $ \ix ->
          let i = A.unindex1 ix
              y1 = y1s A.!! i
              y2 = y2s A.!! i
              s = ss A.!! i
           in isCircle i ? (y1 - s, A.min y1 y2 - s)

      maxYs =
        A.generate sh $ \ix ->
          let i = A.unindex1 ix
              y1 = y1s A.!! i
              y2 = y2s A.!! i
              s = ss A.!! i
           in isCircle i ? (y1 + s, A.max y1 y2 + s)
   in A.lift (tags, x1s, y1s, x2s, y2s, ss, cols, minXs, maxXs, minYs, maxYs)

-- | Single-pass renderer with reverse iteration and early exit.
-- Iterates from last primitive to first per pixel, stopping on first hit.
-- Last primitive wins (correct draw order), with minimal work for covered pixels.
renderAccWithBounds :: Acc InputsWithBounds -> Acc (Array DIM2 Word32)
renderAccWithBounds input =
  let (tags, x1s, y1s, x2s, y2s, ss, cols, minXs, maxXs, minYs, maxYs) =
        ( unlift input ::
            ( Acc (Vector Int32),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float)
            )
        )
      count = A.size tags
   in A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = A.unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x :: Exp Float
            py = A.fromIntegral y :: Exp Float

            -- Start from the last primitive (highest index), iterate backwards
            initial = A.lift (count - 1, A.constant 0xFF000000 :: Exp Word32)

            wcond st =
              let (i, _) = A.unlift st :: (Exp Int, Exp Word32)
               in i A.>= 0

            body st =
              let (i, acc) = A.unlift st :: (Exp Int, Exp Word32)
                  tag = tags A.!! i
                  lx1 = x1s A.!! i
                  ly1 = y1s A.!! i
                  lx2 = x2s A.!! i
                  ly2 = y2s A.!! i
                  s = ss A.!! i
                  col = cols A.!! i

                  bMinX = minXs A.!! i
                  bMaxX = maxXs A.!! i
                  bMinY = minYs A.!! i
                  bMaxY = maxYs A.!! i

                  inBox =
                    px A.>= bMinX
                      A.&& px A.<= bMaxX
                      A.&& py A.>= bMinY
                      A.&& py A.<= bMaxY

                  isCircle = tag A.== A.constant circleTagVal
                  ddx = px - lx1
                  ddy = py - ly1
                  circleHit = ddx * ddx + ddy * ddy A.< s * s
                  lineHit = distToSegmentSq px py lx1 ly1 lx2 ly2 A.< s * s

                  isHit = inBox A.&& (isCircle ? (circleHit, lineHit))

                  newAcc = isHit ? (col, acc)
                  -- Sentinel: set i = -1 on hit to force early exit
                  newI = isHit ? (A.constant (-1), i - 1)
               in A.lift (newI, newAcc)
         in A.snd (A.while wcond body initial)

-- | Render pipeline: precompute bounds, then single-pass reverse iteration.
renderPipeline :: Acc Inputs -> Acc (Array DIM2 Word32)
renderPipeline = renderAccWithBounds . precomputeBounds

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envRender :: Inputs -> Array DIM2 Word32
  }

tick :: Tick
tick env = do
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  let numPrims = 500

      fi :: Int -> Float
      fi j = Prelude.fromIntegral (j + 1)

      pxAt :: Int -> Float
      pxAt j = 160 + 140 * cos (frame / 30 + fi j * 0.1)

      pyAt :: Int -> Float
      pyAt j = 100 + 80 * sin (frame / 50 + fi j * 0.2)

      isCircle :: Int -> Bool
      isCircle j = (j + 1) `Prelude.rem` 2 Prelude.== 0

      colAt :: Int -> Word32
      colAt j = 0xFF000000 + Prelude.fromIntegral (Prelude.floor (fi j * 12345) `Prelude.rem` (0x00FFFFFF :: Int))

      tags = VS.generate numPrims $ \j ->
        if isCircle j then circleTagVal else lineTagVal

      x1s = VS.generate numPrims $ \j ->
        if isCircle j then pxAt j else 160

      y1s = VS.generate numPrims $ \j ->
        if isCircle j then pyAt j else 100

      x2s = VS.generate numPrims $ \j ->
        if isCircle j then 0 else pxAt j

      y2s = VS.generate numPrims $ \j ->
        if isCircle j then 0 else pyAt j

      szs = VS.generate numPrims $ \j ->
        if isCircle j then 5 + 3 * sin (frame / 10 + fi j) else 1

      clrs = VS.generate numPrims colAt

      sh = Z :. numPrims
      inputs =
        ( AVS.fromVectors sh tags,
          AVS.fromVectors sh x1s,
          AVS.fromVectors sh y1s,
          AVS.fromVectors sh x2s,
          AVS.fromVectors sh y2s,
          AVS.fromVectors sh szs,
          AVS.fromVectors sh clrs
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

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envRender = CPU.run1 renderPipeline
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
