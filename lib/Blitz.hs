module Blitz where

import Control.Concurrent (forkOS)
-- import Data.Array.Accelerate.LLVM.Native as CPU

import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
-- import Data.Array.Accelerate.Data.Bits qualified as ABits
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.PTX as GPU
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

circleTag, lineTag :: Exp Int32
circleTag = 0
lineTag = 1

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

-- renderAcc :: Acc (Scalar Int) -> Acc (Array DIM2 Word32)
-- renderAcc t =
--   A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
--     let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
--         f = the t
--         r = A.fromIntegral ((x + f) `A.rem` 256) :: Exp Word32
--         g = A.fromIntegral ((y + f) `A.rem` 256) :: Exp Word32
--         b = A.fromIntegral ((x + y) `A.rem` 256) :: Exp Word32
--         a = 255 :: Exp Word32
--      in (r `ABits.shiftL` 0)
--           ABits..|. (g `ABits.shiftL` 8)
--           ABits..|. (b `ABits.shiftL` 16)
--           ABits..|. (a `ABits.shiftL` 24)

renderAcc :: Acc (Vector Primitive) -> Acc (Array DIM2 Word32)
renderAcc primitives =
  A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
    let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
        px = A.fromIntegral x
        py = A.fromIntegral y

        count = A.size primitives
        initial = lift (A.constant 0 :: Exp Int, A.constant 0xFF000000 :: Exp Word32)

        wcond st =
          let (i, _col) = unlift st :: (Exp Int, Exp Word32)
           in i A.< count

        body st =
          let (i, acc) = unlift st :: (Exp Int, Exp Word32)
              prim = primitives A.!! i
              (tag, x1, y1, x2, y2, s, col) = unlift prim

              dxp = px - x1
              dyp = py - y1

              isHit =
                A.cond
                  (tag A.== circleTag)
                  ((dxp * dxp) + (dyp * dyp) A.< s * s)
                  (distToSegment px py x1 y1 x2 y2 A.< s)

              newAcc = isHit ? (col, acc)
           in lift (i + 1, newAcc)
     in A.snd (A.while wcond body initial)

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envRender :: Array DIM1 Primitive -> Array DIM2 Word32
    -- envRender :: Scalar Int -> Array DIM2 Word32,
  }

tick :: Tick
tick env = do
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float

  let rawScene :: [Primitive]
      rawScene =
        [ (0, 160, 90, 0, 0, 40 + 10 * sin (frame / 10), 0xFF444444),
          (1, 20, 20, 300, 160, 2, 0xFF00FF00),
          (0, 160 + 60 * cos (frame / 20), 90 + 30 * sin (frame / 20), 0, 0, 10, 0xFF0000FF)
        ]

  let (t, x1, y1, x2, y2, s, c) = L.unzip7 rawScene

  -- This structure matches the recursive pair representation ((), a), b), c)...
  -- Based on your error: Expected (((((((), Int32), Float), Float), Float), Float), Float), Word32)
  let sceneVectors =
        ( ( ( ( ( ( ( (),
                      VS.fromList t
                    ),
                    VS.fromList x1
                  ),
                  VS.fromList y1
                ),
                VS.fromList x2
              ),
              VS.fromList y2
            ),
            VS.fromList s
          ),
          VS.fromList c
        )

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

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  frameRef <- newIORef (0 :: Int)

  let env =
        Env
          { envWindow = window,
            envTex = tex,
            envFrameRef = frameRef,
            envRender = GPU.run1 renderAcc
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
