module Blitz where

import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
-- import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.IORef
import Data.List qualified as L
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

-- Maximum bounding box side length for circle permute pass.
-- Must accommodate max diameter (2 * max_radius) plus rounding margin.
-- Max radius = 5 + 3 = 8, diameter = 16, plus margin = 20.
maxBBSide :: Int
maxBBSide = 20

-- Pack (originalIndex, color) into Word64 for priority-correct compositing.
-- High 32 bits = index + 1, low 32 bits = color.
-- Background encodes with high bits = 0, so any real primitive (index >= 0) wins via max.
encodePixel :: Exp Int32 -> Exp Word32 -> Exp Word64
encodePixel idx col =
  A.fromIntegral (idx + 1) * 0x100000000 + A.fromIntegral col

decodeColor :: Exp Word64 -> Exp Word32
decodeColor packed = A.fromIntegral packed

bgEncoded :: Exp Word64
bgEncoded = encodePixel (A.constant (-1)) (A.constant 0xFF000000)

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

-- Circle inputs: (cx, cy, radius, color, originalIndex)
type CircleInputs =
  ( Vector Float,
    Vector Float,
    Vector Float,
    Vector Word32,
    Vector Int32
  )

-- Line inputs: (x1, y1, x2, y2, thickness, color, originalIndex)
type LineInputs =
  ( Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Word32,
    Vector Int32
  )

type RenderInputs = (CircleInputs, LineInputs)

-- | Render circles by scattering pixel values via permute.
-- For each circle, generates a bounding-box-sized grid and tests each pixel.
-- Uses permute with max to resolve draw order (highest original index wins).
-- Work: O(numCircles * maxBBSide^2) instead of O(fbPixels * numCircles).
renderCircles :: Acc CircleInputs -> Acc (Array DIM2 Word64)
renderCircles circleInput =
  let (cxs, cys, rads, cols, origIdxs) =
        ( unlift circleInput ::
            ( Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32),
              Acc (Vector Int32)
            )
        )
      numCircles = A.size cxs
      maxBB = A.constant maxBBSide

      srcShape = A.lift (Z :. numCircles :. maxBB :. maxBB)

      source = A.generate srcShape $ \ix ->
        let Z :. ci :. _ :. _ = A.unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
            col = cols A.!! ci
            origI = origIdxs A.!! ci
         in encodePixel origI col

      defaults = A.fill (A.constant (Z :. fbH :. fbW)) bgEncoded

      indexFn ix =
        let Z :. ci :. ly :. lx = A.unlift ix :: Z :. Exp Int :. Exp Int :. Exp Int
            cx = cxs A.!! ci
            cy = cys A.!! ci
            rad = rads A.!! ci

            halfBB = A.constant (maxBBSide `Prelude.div` 2) :: Exp Int
            gx = (A.round cx :: Exp Int) + lx - halfBB
            gy = (A.round cy :: Exp Int) + ly - halfBB

            px = A.fromIntegral gx :: Exp Float
            py = A.fromIntegral gy :: Exp Float
            ddx = px - cx
            ddy = py - cy
            inCircle = ddx * ddx + ddy * ddy A.< rad * rad

            inBounds =
              gx A.>= 0
                A.&& gx A.< A.constant fbW
                A.&& gy A.>= 0
                A.&& gy A.< A.constant fbH
         in A.cond
              (inCircle A.&& inBounds)
              (Just_ (A.lift (Z :. gy :. gx)))
              Nothing_
   in A.permute A.max defaults indexFn source

-- | Render lines using per-pixel reverse iteration with early exit.
-- Iterates from last line to first; first hit = highest original index = correct.
-- Work: O(fbPixels * avgLinesChecked), with early exit for covered pixels.
renderLines :: Acc LineInputs -> Acc (Array DIM2 Word64)
renderLines lineInput =
  let (lx1s, ly1s, lx2s, ly2s, lthicks, lcols, lorigIdxs) =
        ( unlift lineInput ::
            ( Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32),
              Acc (Vector Int32)
            )
        )
      numLines = A.size lx1s

      -- Precompute bounding boxes for lines
      sh1 = A.index1 numLines

      minXs =
        A.generate sh1 $ \ix ->
          let i = A.unindex1 ix
              x1 = lx1s A.!! i
              x2 = lx2s A.!! i
              s = lthicks A.!! i
           in A.min x1 x2 - s

      maxXs =
        A.generate sh1 $ \ix ->
          let i = A.unindex1 ix
              x1 = lx1s A.!! i
              x2 = lx2s A.!! i
              s = lthicks A.!! i
           in A.max x1 x2 + s

      minYs =
        A.generate sh1 $ \ix ->
          let i = A.unindex1 ix
              y1 = ly1s A.!! i
              y2 = ly2s A.!! i
              s = lthicks A.!! i
           in A.min y1 y2 - s

      maxYs =
        A.generate sh1 $ \ix ->
          let i = A.unindex1 ix
              y1 = ly1s A.!! i
              y2 = ly2s A.!! i
              s = lthicks A.!! i
           in A.max y1 y2 + s
   in A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = A.unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x :: Exp Float
            py = A.fromIntegral y :: Exp Float

            -- Reverse iteration: start from last line, exit on first hit
            initial = A.lift (numLines - 1, bgEncoded)

            -- Continue while no hit found and lines remain.
            -- enc < 0x100000000 means high 32 bits are 0 (no real primitive hit yet).
            wcond st =
              let (i, enc) = A.unlift st :: (Exp Int, Exp Word64)
               in i A.>= 0 A.&& enc A.< 0x100000000

            body st =
              let (i, acc) = A.unlift st :: (Exp Int, Exp Word64)
                  x1 = lx1s A.!! i
                  y1 = ly1s A.!! i
                  x2 = lx2s A.!! i
                  y2 = ly2s A.!! i
                  s = lthicks A.!! i
                  col = lcols A.!! i
                  origI = lorigIdxs A.!! i

                  bMinX = minXs A.!! i
                  bMaxX = maxXs A.!! i
                  bMinY = minYs A.!! i
                  bMaxY = maxYs A.!! i

                  inBox =
                    px A.>= bMinX
                      A.&& px A.<= bMaxX
                      A.&& py A.>= bMinY
                      A.&& py A.<= bMaxY

                  s2 = s * s
                  isHit = inBox A.&& distToSegmentSq px py x1 y1 x2 y2 A.< s2

                  newAcc = isHit ? (encodePixel origI col, acc)
               in A.lift (i - 1, newAcc)
         in A.snd (A.while wcond body initial)

-- | Composite circle and line framebuffers.
-- Takes max of encoded values (highest original index wins), then decodes to Word32.
composite ::
  Acc (Array DIM2 Word64) ->
  Acc (Array DIM2 Word64) ->
  Acc (Array DIM2 Word32)
composite circleFB lineFB =
  A.zipWith (\c l -> decodeColor (A.max c l)) circleFB lineFB

-- | Two-pass render pipeline: permute-based circles + reverse-iteration lines.
renderPipeline :: Acc RenderInputs -> Acc (Array DIM2 Word32)
renderPipeline input =
  let (circleInput, lineInput) = A.unlift input
   in composite (renderCircles circleInput) (renderLines lineInput)

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envRender :: RenderInputs -> Array DIM2 Word32
  }

tick :: Tick
tick env = do
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  let numPrims = 500
      rawScene = Prelude.map (genPrim frame) [1 .. numPrims]

      genPrim f2 i =
        let fi = Prelude.fromIntegral i
            x = 160 + 140 * cos (f2 / 30 + fi * 0.1)
            y = 100 + 80 * sin (f2 / 50 + fi * 0.2)
            col = 0xFF000000 + (Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` (0x00FFFFFF :: Int)))
         in if i `Prelude.rem` 2 Prelude.== 0
              then (circleTagVal, x, y, 0, 0, 5 + 3 * sin (f2 / 10 + fi), col)
              else (lineTagVal, 160, 100, x, y, 1, col)

      -- Split into circles and lines, preserving original draw indices
      indexedScene = Prelude.zip [(0 :: Int32) ..] rawScene

      circleData =
        [ (cx, cy, sz, col, idx)
          | (idx, (tag, cx, cy, _, _, sz, col)) <- indexedScene,
            tag Prelude.== circleTagVal
        ]
      lineData =
        [ (lx1, ly1, lx2, ly2, sz, col, idx)
          | (idx, (tag, lx1, ly1, lx2, ly2, sz, col)) <- indexedScene,
            tag Prelude.== lineTagVal
        ]

      (cXs, cYs, cRs, cCols, cIdxs) = L.unzip5 circleData
      (lX1s, lY1s, lX2s, lY2s, lTs, lCols, lIdxs) = L.unzip7 lineData

      numCircles = Prelude.length circleData
      numLns = Prelude.length lineData

      shC = Z :. numCircles
      shL = Z :. numLns

      circleInputs =
        ( A.fromList shC cXs,
          A.fromList shC cYs,
          A.fromList shC cRs,
          A.fromList shC cCols,
          A.fromList shC cIdxs
        )
      lineInputs =
        ( A.fromList shL lX1s,
          A.fromList shL lY1s,
          A.fromList shL lX2s,
          A.fromList shL lY2s,
          A.fromList shL lTs,
          A.fromList shL lCols,
          A.fromList shL lIdxs
        )

  let arr = env.envRender (circleInputs, lineInputs)
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
