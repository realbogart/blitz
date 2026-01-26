module Blitz where

import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Bits qualified as ABits
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

tileSize :: Int
tileSize = 16

tilesW, tilesH :: Int
tilesW = fbW `div` tileSize -- 320 / 16 = 20
tilesH = fbH `div` tileSize -- 200 / 16 = 12.5 -> 13

maxPrimsPerTile :: Int
maxPrimsPerTile = 64

-- (Tag, x1, y1, x2, y2, Size/Thickness, Color)
type Primitive = (Int32, Float, Float, Float, Float, Float, Word32)

-- Plain values for the CPU
circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

-- GPU Expressions for the Shader
circleTag, lineTag :: Exp Int32
circleTag = A.constant circleTagVal
lineTag = A.constant lineTagVal

-- | Distance from point (px, py) to line segment (x1, y1) -> (x2, y2)
distToSegment :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
distToSegment px py x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
      t = A.max 0 (A.min 1 (((px - x1) * dx + (py - y1) * dy) / l2))
      projX = x1 + t * dx
      projY = y1 + t * dy
      dxp = px - projX
      dyp = py - projY
   in A.sqrt (dxp * dxp + dyp * dyp)

renderAcc :: Acc (Vector Primitive) -> Acc (Array DIM2 Word32)
renderAcc primitives =
  A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
    let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
        px = A.fromIntegral x
        py = A.fromIntegral y

        count = A.size primitives
        initial = lift (A.constant 0 :: Exp Int, A.constant 0xFF000000 :: Exp Word32)

        wcond st = let (i, _) = unlift st :: (Exp Int, Exp Word32) in i A.< count

        body st =
          let (i, acc) = unlift st :: (Exp Int, Exp Word32)
              prim = primitives A.!! i
              (tag, x1, y1, x2, y2, s, col) = unlift prim

              -- 1. Calculate the Bounding Box
              -- For circles, it's center +/- radius.
              -- For lines, it's min/max of endpoints +/- thickness.
              minX = A.cond (tag A.== circleTag) (x1 - s) (A.min x1 x2 - s)
              maxX = A.cond (tag A.== circleTag) (x1 + s) (A.max x1 x2 + s)
              minY = A.cond (tag A.== circleTag) (y1 - s) (A.min y1 y2 - s)
              maxY = A.cond (tag A.== circleTag) (y1 + s) (A.max y1 y2 + s)

              -- 2. The Short-Circuit Check
              inBox = px A.>= minX A.&& px A.<= maxX A.&& py A.>= minY A.&& py A.<= maxY

              -- 3. Only run expensive math if inside the box
              dxp = px - x1
              dyp = py - y1

              newAcc =
                inBox
                  ? ( A.cond
                        (tag A.== circleTag)
                        ((dxp * dxp) + (dyp * dyp) A.< s * s ? (col, acc))
                        (distToSegment px py x1 y1 x2 y2 A.< s ? (col, acc)),
                      acc
                    )
           in lift (i + 1, newAcc)
     in A.snd (A.while wcond body initial)

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envRender :: Array DIM1 Primitive -> Array DIM2 Word32
  }

tick :: Tick
tick env = do
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  -- Stress Test: Generate 500 primitives
  let numPrims = 500
      rawScene = Prelude.map genPrim [1 .. numPrims]

      genPrim i =
        let fi = Prelude.fromIntegral i
            x = 160 + 140 * cos (frame / 30 + fi * 0.1)
            y = 100 + 80 * sin (frame / 50 + fi * 0.2)
            col =
              0xFF000000
                + (Prelude.fromIntegral (Prelude.floor (fi * 12345) `Prelude.rem` 0x00FFFFFF))
         in if i `Prelude.rem` 2 Prelude.== 0
              then (circleTagVal, x, y, 0, 0, 5 + 3 * sin (frame / 10 + fi), col)
              else (lineTagVal, 160, 100, x, y, 1, col)

  -- The conversion logic remains the same
  let (t, x1, y1, x2, y2, s, c) = L.unzip7 rawScene
  let sceneVectors = ((((((((), VS.fromList t), VS.fromList x1), VS.fromList y1), VS.fromList x2), VS.fromList y2), VS.fromList s), VS.fromList c)

  let accScene = AVS.fromVectors (Z :. L.length rawScene) sceneVectors
  let arr = env.envRender accScene
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
  -- toggleFullscreen

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  frameRef <- newIORef (0 :: Int)

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envRender = CPU.run1 renderAcc
          }

  gameLoop env tickRef
    `finally` do
      unloadTexture tex window
      closeWindow (Just window)

gameLoop :: Env -> IORef Tick -> IO ()
gameLoop env tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn env
    gameLoop env tickRef
