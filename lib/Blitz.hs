module Blitz where

import Control.Concurrent (forkOS)
import Control.Exception (finally)
import Control.Monad
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits qualified as ABits
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
-- import Data.Array.Accelerate.LLVM.PTX as GPU
import Data.Bits qualified as DBits
import Data.IORef
-- import Data.Int (Int32)
import Data.List qualified as L
import Data.Vector.Storable qualified as VS
-- import Data.Word (Word32)
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

-- (Tag, x1, y1, x2, y2, Size/Thickness, Color, MortonCode)
type Primitive = (Int32, Float, Float, Float, Float, Float, Word32, Word32)

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

circleTag, lineTag :: Exp Int32
circleTag = A.constant circleTagVal
lineTag = A.constant lineTagVal

--------------------------------------------------------------------------------
-- Spatial Hashing (Morton Codes)
--------------------------------------------------------------------------------

part1by1 :: Exp Word32 -> Exp Word32
part1by1 n =
  let n1 = (n ABits..|. (n `ABits.shiftL` 8)) ABits..&. 0x00FF00FF
      n2 = (n1 ABits..|. (n1 `ABits.shiftL` 4)) ABits..&. 0x0F0F0F0F
      n3 = (n2 ABits..|. (n2 `ABits.shiftL` 2)) ABits..&. 0x33333333
      n4 = (n3 ABits..|. (n3 `ABits.shiftL` 1)) ABits..&. 0x55555555
   in n4

morton2D_GPU :: Exp Int32 -> Exp Int32 -> Exp Word32
morton2D_GPU x y =
  let x' = A.fromIntegral x :: Exp Word32
      y' = A.fromIntegral y :: Exp Word32
   in (part1by1 x') ABits..|. ((part1by1 y') `ABits.shiftL` 1)

part1by1_CPU :: Word32 -> Word32
part1by1_CPU n =
  let n1 = (n `DBits.xor` (n `DBits.shiftL` 8)) DBits..&. 0x00FF00FF
      n2 = (n1 `DBits.xor` (n1 `DBits.shiftL` 4)) DBits..&. 0x0F0F0F0F
      n3 = (n2 `DBits.xor` (n2 `DBits.shiftL` 2)) DBits..&. 0x33333333
      n4 = (n3 `DBits.xor` (n3 `DBits.shiftL` 1)) DBits..&. 0x55555555
   in n4

getMorton_CPU :: Int32 -> Float -> Float -> Float -> Float -> Word32
getMorton_CPU tag x1 y1 x2 y2 =
  let cx = if tag Prelude.== 0 then x1 else (x1 + x2) / 2
      cy = if tag Prelude.== 0 then y1 else (y1 + y2) / 2
      mx = Prelude.floor (Prelude.max 0 (Prelude.min 65535 (cx / Prelude.fromIntegral fbW * 65535)))
      my = Prelude.floor (Prelude.max 0 (Prelude.min 65535 (cy / Prelude.fromIntegral fbH * 65535)))
   in (part1by1_CPU mx) DBits..|. (part1by1_CPU my `DBits.shiftL` 1)

-- Note: using ior and setBit from Data.Bits for CPU side.

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

distToSegment :: Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float -> Exp Float
distToSegment px py x1 y1 x2 y2 =
  let dx = x2 - x1
      dy = y2 - y1
      l2 = dx * dx + dy * dy
   in A.cond
        (l2 A.< 0.0001)
        (A.sqrt ((px - x1) * (px - x1) + (py - y1) * (py - y1))) -- Treat as point
        ( let t = A.max 0 (A.min 1 (((px - x1) * dx + (py - y1) * dy) / l2))
              projX = x1 + t * dx
              projY = y1 + t * dy
              dxp = px - projX
              dyp = py - projY
           in A.sqrt (dxp * dxp + dyp * dyp)
        )

renderAcc ::
  Acc
    ( Vector Int32,
      Vector Float,
      Vector Float,
      Vector Float,
      Vector Float,
      Vector Float,
      Vector Word32,
      Vector Word32
    ) ->
  Acc (Array DIM2 Word32)
renderAcc input =
  let (tags, x1s, y1s, x2s, y2s, ss, cols, mortons) = unlift input
   in A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x
            py = A.fromIntegral y
            targetMorton = morton2D_GPU (A.fromIntegral (x `div` 32)) (A.fromIntegral (y `div` 32))
            count = A.size tags

            -- Binary Search
            bsInitial = lift (A.constant 0 :: Exp Int, count)
            bsCond st = let (low, high) = unlift st :: (Exp Int, Exp Int) in low A.< high
            bsBody st =
              let (low, high) = unlift st :: (Exp Int, Exp Int)
                  mid = (low + high) `div` 2
                  mCode = mortons A.!! mid
               in (mCode A.< targetMorton) ? (lift (mid + 1, high), lift (low, mid))

            (startIdx, _) = unlift (A.while bsCond bsBody bsInitial) :: (Exp Int, Exp Int)

            -- Loop
            initial = lift (startIdx, A.constant 0xFF000000 :: Exp Word32, A.constant False :: Exp Bool)
            wcond st =
              let (i, _, _) = unlift st :: (Exp Int, Exp Word32, Exp Bool)
               in i A.< count

            body st =
              let (i, acc, _) = unlift st :: (Exp Int, Exp Word32, Exp Bool)
                  tag = tags A.!! i
                  x1 = x1s A.!! i
                  y1 = y1s A.!! i
                  x2 = x2s A.!! i
                  y2 = y2s A.!! i
                  s = ss A.!! i
                  col = cols A.!! i

                  isCircle = tag A.== circleTag

                  -- Use distToSegment only for lines, simple dist for circles
                  dist =
                    isCircle
                      ? ( A.sqrt ((px - x1) * (px - x1) + (py - y1) * (py - y1)),
                          distToSegment px py x1 y1 x2 y2
                        )

                  -- The Hit check
                  isHit = dist A.< s

                  -- Only overwrite if it's actually a hit, otherwise keep background
                  newAcc = isHit ? (col, acc)
               in lift (i + 1, newAcc, A.constant False)
         in let (_, finalCol, _) = unlift (A.while wcond body initial) :: (Exp Int, Exp Word32, Exp Bool)
             in finalCol

--------------------------------------------------------------------------------
-- Environment & Loop
--------------------------------------------------------------------------------

data Env = Env
  { envWindow :: WindowResources,
    envTex :: Texture,
    envFrameRef :: IORef Int,
    envRender ::
      ( Vector Int32,
        Vector Float,
        Vector Float,
        Vector Float,
        Vector Float,
        Vector Float,
        Vector Word32,
        Vector Word32
      ) ->
      Array DIM2 Word32
  }

tick :: Tick
tick env = do
  f <- readIORef env.envFrameRef
  modifyIORef' env.envFrameRef (+ 1)
  let frame = Prelude.fromIntegral f :: Float
      numPrims = 500

      genPrim i =
        let fi = Prelude.fromIntegral i
            -- Use fbW/fbH instead of hardcoded 160/100
            w = Prelude.fromIntegral fbW
            h = Prelude.fromIntegral fbH
            x = (w / 2) + (w / 3) * Prelude.cos (frame / 30 + fi * 0.1)
            y = (h / 2) + (h / 3) * Prelude.sin (frame / 50 + fi * 0.2)
            tag = if i `Prelude.rem` 2 Prelude.== 0 then circleTagVal else lineTagVal
            -- Bright colors to verify visibility
            col = 0xFF00FFFF -- Yellow (RGBA/ABGR depending on platform)
            (x2, y2) = if tag Prelude.== circleTagVal then (x, y) else (w / 2, h / 2)
            s = if tag Prelude.== circleTagVal then 3 + 2 * Prelude.sin (frame / 10 + fi) else 1
            m = getMorton_CPU tag x y x2 y2
         in (tag, x, y, x2, y2, s, col, m)

      rawScene = L.sortOn (\(_, _, _, _, _, _, _, m) -> m) (Prelude.map genPrim [1 .. numPrims])

  -- Manually unzip the 8-tuple list
  -- Ensure this order matches (tag, x1, y1, x2, y2, s, col, m)
  let (lt, lx1, ly1, lx2, ly2, ls, lc, lm) =
        L.foldr
          ( \(t, x1, y1, x2, y2, s, c, m) (ts, x1s, y1s, x2s, y2s, ss, cs, ms) ->
              (t : ts, x1 : x1s, y1 : y1s, x2 : x2s, y2 : y2s, s : ss, c : cs, m : ms)
          )
          ([], [], [], [], [], [], [], [])
          rawScene

  -- Convert each list to a separate Accelerate Array (Plain Array, not Acc)
  let n = L.length rawScene
      sh = Z :. n
      accTags = A.fromList sh lt
      accX1 = A.fromList sh lx1
      accY1 = A.fromList sh ly1
      accX2 = A.fromList sh lx2
      accY2 = A.fromList sh ly2
      accS = A.fromList sh ls
      accC = A.fromList sh lc
      accM = A.fromList sh lm

  -- Use the compiled render function from env
  let arr = (env.envRender) (accTags, accX1, accY1, accX2, accY2, accS, accC, accM)
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
  frameRef <- newIORef 0

  let env = Env window tex frameRef (CPU.run1 renderAcc)

  gameLoop env tickRef `finally` do
    unloadTexture tex window
    closeWindow (Just window)

gameLoop :: Env -> IORef Tick -> IO ()
gameLoop env tickRef = do
  sc <- windowShouldClose
  unless sc $ do
    tickFn <- readIORef tickRef
    tickFn env
    gameLoop env tickRef
