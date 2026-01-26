module Blitz where

import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
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

-- (Tag, x1, y1, x2, y2, Size/Thickness, Color)
type Primitive = (Int32, Float, Float, Float, Float, Float, Word32)

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

circleTag, lineTag :: Exp Int32
circleTag = A.constant circleTagVal
lineTag = A.constant lineTagVal

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

type Inputs =
  ( Vector Int32, -- tags
    Vector Float, -- x1s
    Vector Float, -- y1s
    Vector Float, -- x2s
    Vector Float, -- y2s
    Vector Float, -- sizes
    Vector Word32 -- colors
  )

type InputsWithBounds =
  ( Vector Int32, -- tags
    Vector Float, -- x1s
    Vector Float, -- y1s
    Vector Float, -- x2s
    Vector Float, -- y2s
    Vector Float, -- sizes
    Vector Word32, -- colors
    Vector Float, -- minXs
    Vector Float, -- maxXs
    Vector Float, -- minYs
    Vector Float -- maxYs
  )

precomputeBounds :: Acc Inputs -> Acc InputsWithBounds
precomputeBounds input =
  let ( tags,
        x1s,
        y1s,
        x2s,
        y2s,
        ss,
        cols
        ) =
          (unlift input :: (Acc (Vector Int32), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Float), Acc (Vector Word32)))

      n = A.size tags
      sh1 = index1 n

      minXs =
        A.generate sh1 $ \ix ->
          let i = unindex1 ix
              tag = tags A.!! i
              x1 = x1s A.!! i
              x2 = x2s A.!! i
              s = ss A.!! i
           in (tag A.== circleTag) ? (x1 - s, A.min x1 x2 - s)

      maxXs =
        A.generate sh1 $ \ix ->
          let i = unindex1 ix
              tag = tags A.!! i
              x1 = x1s A.!! i
              x2 = x2s A.!! i
              s = ss A.!! i
           in (tag A.== circleTag) ? (x1 + s, A.max x1 x2 + s)

      minYs =
        A.generate sh1 $ \ix ->
          let i = unindex1 ix
              tag = tags A.!! i
              y1 = y1s A.!! i
              y2 = y2s A.!! i
              s = ss A.!! i
           in (tag A.== circleTag) ? (y1 - s, A.min y1 y2 - s)

      maxYs =
        A.generate sh1 $ \ix ->
          let i = unindex1 ix
              tag = tags A.!! i
              y1 = y1s A.!! i
              y2 = y2s A.!! i
              s = ss A.!! i
           in (tag A.== circleTag) ? (y1 + s, A.max y1 y2 + s)
   in lift (tags, x1s, y1s, x2s, y2s, ss, cols, minXs, maxXs, minYs, maxYs)

renderAccWithBounds :: Acc InputsWithBounds -> Acc (Array DIM2 Word32)
renderAccWithBounds input =
  let ( tags,
        x1s,
        y1s,
        x2s,
        y2s,
        ss,
        cols,
        minXs,
        maxXs,
        minYs,
        maxYs
        ) =
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
   in A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x
            py = A.fromIntegral y

            count = A.size tags
            initial = lift (A.constant 0 :: Exp Int, A.constant 0xFF000000 :: Exp Word32)

            wcond st =
              let (i, _) = unlift st :: (Exp Int, Exp Word32)
               in i A.< count

            body st =
              let (i, acc) = unlift st :: (Exp Int, Exp Word32)
                  tag = tags A.!! i
                  x1 = x1s A.!! i
                  y1 = y1s A.!! i
                  x2 = x2s A.!! i
                  y2 = y2s A.!! i
                  s = ss A.!! i
                  col = cols A.!! i

                  minX = minXs A.!! i
                  maxX = maxXs A.!! i
                  minY = minYs A.!! i
                  maxY = maxYs A.!! i

                  inBox = px A.>= minX A.&& px A.<= maxX A.&& py A.>= minY A.&& py A.<= maxY

                  dxp = px - x1
                  dyp = py - y1
                  s2 = s * s

                  isHit =
                    inBox
                      A.&& ( (tag A.== circleTag)
                               ? ( (dxp * dxp) + (dyp * dyp) A.< s2,
                                   distToSegmentSq px py x1 y1 x2 y2 A.< s2
                                 )
                           )

                  newAcc = isHit ? (col, acc)
               in lift (i + 1, newAcc)
         in A.snd (A.while wcond body initial)

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
      rawScene = Prelude.map (genPrim frame) [1 .. numPrims]

      genPrim f2 i =
        let fi = Prelude.fromIntegral i
            x = 160 + 140 * cos (f2 / 30 + fi * 0.1)
            y = 100 + 80 * sin (f2 / 50 + fi * 0.2)
            col = 0xFF000000 + (Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` 0x00FFFFFF))
         in if i `Prelude.rem` 2 Prelude.== 0
              then (circleTagVal, x, y, 0, 0, 5 + 3 * sin (f2 / 10 + fi), col)
              else (lineTagVal, 160, 100, x, y, 1, col)

  let (t, x1, y1, x2, y2, s, c) = L.unzip7 rawScene
      sh = Z :. numPrims
      accT = A.fromList sh t
      accX1 = A.fromList sh x1
      accY1 = A.fromList sh y1
      accX2 = A.fromList sh x2
      accY2 = A.fromList sh y2
      accS = A.fromList sh s
      accC = A.fromList sh c

  let arr = env.envRender (accT, accX1, accY1, accX2, accY2, accS, accC)
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
