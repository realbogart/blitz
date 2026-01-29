module Blitz
  ( Resources,
    initResources,
    renderFrame,
    renderFrameCached,
    withFramePixels,
    withFramePixelsCached,
    fbW,
    fbH,
    numPrims,
    tileW,
    tileH,
    tilesX,
    tilesY,
    numTiles,
    maxPrimsPerTile,
    circleTagVal,
    lineTagVal,
  )
where

import Blitz.Draw (DrawM, runDrawFrame)
import Control.Monad (when)
import Data.Array.Accelerate as A
import Data.Array.Accelerate.IO.Data.Vector.Storable qualified as AVS
import Data.Array.Accelerate.LLVM.Native as CPU
import Data.IORef
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Foreign.Ptr (Ptr)

fbW, fbH :: Int
fbW = 320
fbH = 200

numPrims :: Int
numPrims = 4000

tileW, tileH, tilesX, tilesY, numTiles, maxPrimsPerTile :: Int
tileW = 16
tileH = 16
tilesX = (fbW + tileW - 1) `Prelude.div` tileW
tilesY = (fbH + tileH - 1) `Prelude.div` tileH
numTiles = tilesX * tilesY
maxPrimsPerTile = 256

circleTagVal, lineTagVal :: Int32
circleTagVal = 0
lineTagVal = 1

data Resources = Resources
  { resTagsV :: VSM.IOVector Int32,
    resX1sV :: VSM.IOVector Float,
    resY1sV :: VSM.IOVector Float,
    resX2sV :: VSM.IOVector Float,
    resY2sV :: VSM.IOVector Float,
    resSizesV :: VSM.IOVector Float,
    resColorsV :: VSM.IOVector Word32,
    resTileCountsV :: VSM.IOVector Int32,
    resTileBinsV :: VSM.IOVector Int32,
    resTags :: VS.Vector Int32,
    resX1s :: VS.Vector Float,
    resY1s :: VS.Vector Float,
    resX2s :: VS.Vector Float,
    resY2s :: VS.Vector Float,
    resSizes :: VS.Vector Float,
    resInputs :: Inputs,
    resRender :: Inputs -> Array DIM2 Word32,
    resLastCount :: IORef Int
  }

initResources :: IO Resources
initResources = do
  tagsV <- VSM.new numPrims
  x1sV <- VSM.new numPrims
  y1sV <- VSM.new numPrims
  x2sV <- VSM.new numPrims
  y2sV <- VSM.new numPrims
  sizesV <- VSM.new numPrims
  colorsV <- VSM.new numPrims
  tileCountsV <- VSM.new numTiles
  tileBinsV <- VSM.new (numTiles * maxPrimsPerTile)

  tags <- VS.unsafeFreeze tagsV
  x1s <- VS.unsafeFreeze x1sV
  y1s <- VS.unsafeFreeze y1sV
  x2s <- VS.unsafeFreeze x2sV
  y2s <- VS.unsafeFreeze y2sV
  sizes <- VS.unsafeFreeze sizesV
  colors <- VS.unsafeFreeze colorsV
  tileCounts <- VS.unsafeFreeze tileCountsV
  tileBins <- VS.unsafeFreeze tileBinsV

  let sh = Z :. numPrims
      shTileCountsShape = Z :. numTiles
      shTileBinsShape = Z :. (numTiles * maxPrimsPerTile)
      inputs =
        ( AVS.fromVectors sh tags,
          AVS.fromVectors sh x1s,
          AVS.fromVectors sh y1s,
          AVS.fromVectors sh x2s,
          AVS.fromVectors sh y2s,
          AVS.fromVectors sh sizes,
          AVS.fromVectors sh colors,
          AVS.fromVectors shTileCountsShape tileCounts,
          AVS.fromVectors shTileBinsShape tileBins
        )

  lastCount <- newIORef 0
  pure
    Resources
      { resTagsV = tagsV,
        resX1sV = x1sV,
        resY1sV = y1sV,
        resX2sV = x2sV,
        resY2sV = y2sV,
        resSizesV = sizesV,
        resColorsV = colorsV,
        resTileCountsV = tileCountsV,
        resTileBinsV = tileBinsV,
        resTags = tags,
        resX1s = x1s,
        resY1s = y1s,
        resX2s = x2s,
        resY2s = y2s,
        resSizes = sizes,
        resInputs = inputs,
        resRender = CPU.run1 renderPipeline,
        resLastCount = lastCount
      }

renderFrame :: Resources -> DrawM a -> IO (VS.Vector Word32)
renderFrame res action = renderFrameWith res (Just action)

renderFrameCached :: Resources -> IO (VS.Vector Word32)
renderFrameCached res = renderFrameWith res Nothing

withFramePixels :: Resources -> DrawM a -> (Ptr Word32 -> IO b) -> IO b
withFramePixels res action k = do
  vec <- renderFrame res action
  VS.unsafeWith vec k

withFramePixelsCached :: Resources -> (Ptr Word32 -> IO b) -> IO b
withFramePixelsCached res k = do
  vec <- renderFrameCached res
  VS.unsafeWith vec k

renderFrameWith :: Resources -> Maybe (DrawM a) -> IO (VS.Vector Word32)
renderFrameWith res action = do
  nPrimsDrawn <- case action of
    Just drawAction -> do
      n <-
        runDrawFrame
          res.resTagsV
          res.resX1sV
          res.resY1sV
          res.resX2sV
          res.resY2sV
          res.resSizesV
          res.resColorsV
          numPrims
          circleTagVal
          lineTagVal
          drawAction
      writeIORef res.resLastCount n
      pure n
    Nothing -> readIORef res.resLastCount

  buildTileBins
    nPrimsDrawn
    res.resTags
    res.resX1s
    res.resY1s
    res.resX2s
    res.resY2s
    res.resSizes
    res.resTileCountsV
    res.resTileBinsV

  let arr = res.resRender res.resInputs
  pure (AVS.toVectors arr)

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

buildTileBins ::
  Int ->
  VS.Vector Int32 ->
  VS.Vector Float ->
  VS.Vector Float ->
  VS.Vector Float ->
  VS.Vector Float ->
  VS.Vector Float ->
  VSM.IOVector Int32 ->
  VSM.IOVector Int32 ->
  IO ()
buildTileBins nPrims tags x1s y1s x2s y2s ss mTileCounts mTileBins = do
  VSM.set mTileCounts 0
  let maxX = Prelude.fromIntegral (fbW - 1) :: Float
      maxY = Prelude.fromIntegral (fbH - 1) :: Float
      clampInt lo hi v = Prelude.max lo (Prelude.min hi v)
      goPrim !p
        | p Prelude.>= nPrims = pure ()
        | otherwise = do
            let tag = VS.unsafeIndex tags p
                x1 = VS.unsafeIndex x1s p
                y1 = VS.unsafeIndex y1s p
                x2 = VS.unsafeIndex x2s p
                y2 = VS.unsafeIndex y2s p
                s = VS.unsafeIndex ss p
                (minX0, maxX0) =
                  if tag Prelude.== circleTagVal
                    then (x1 - s, x1 + s)
                    else (Prelude.min x1 x2 - s, Prelude.max x1 x2 + s)
                (minY0, maxY0) =
                  if tag Prelude.== circleTagVal
                    then (y1 - s, y1 + s)
                    else (Prelude.min y1 y2 - s, Prelude.max y1 y2 + s)
            if maxX0 Prelude.< 0
              Prelude.|| maxY0 Prelude.< 0
              Prelude.|| minX0 Prelude.> maxX
              Prelude.|| minY0 Prelude.> maxY
              then goPrim (p + 1)
              else do
                let minX = Prelude.max 0 minX0
                    maxX' = Prelude.min maxX maxX0
                    minY = Prelude.max 0 minY0
                    maxY' = Prelude.min maxY maxY0
                    tx0 = clampInt 0 (tilesX - 1) (Prelude.floor minX `Prelude.div` tileW)
                    tx1 = clampInt 0 (tilesX - 1) (Prelude.floor maxX' `Prelude.div` tileW)
                    ty0 = clampInt 0 (tilesY - 1) (Prelude.floor minY `Prelude.div` tileH)
                    ty1 = clampInt 0 (tilesY - 1) (Prelude.floor maxY' `Prelude.div` tileH)
                    goTy !ty
                      | ty Prelude.> ty1 = pure ()
                      | otherwise = do
                          let rowBase = ty * tilesX
                              goTx !tx
                                | tx Prelude.> tx1 = pure ()
                                | otherwise = do
                                    let t = rowBase + tx
                                    c <- VSM.unsafeRead mTileCounts t
                                    let cInt = Prelude.fromIntegral c :: Int
                                    when (cInt Prelude.< maxPrimsPerTile) $ do
                                      VSM.unsafeWrite mTileBins (t * maxPrimsPerTile + cInt) (Prelude.fromIntegral p)
                                      VSM.unsafeWrite mTileCounts t (Prelude.fromIntegral (cInt + 1))
                                    goTx (tx + 1)
                          goTx tx0
                          goTy (ty + 1)
                goTy ty0
                goPrim (p + 1)
  goPrim 0

type Inputs =
  ( Vector Int32,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Float,
    Vector Word32,
    Vector Int32,
    Vector Int32
  )

renderPipeline :: Acc Inputs -> Acc (Array DIM2 Word32)
renderPipeline input =
  let (tags, x1s, y1s, x2s, y2s, ss, cols, tileCounts, tileBinsFlat) =
        ( A.unlift input ::
            ( Acc (Vector Int32),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Float),
              Acc (Vector Word32),
              Acc (Vector Int32),
              Acc (Vector Int32)
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
   in -- Pixel shader: iterate only the compact bin for this tile
      A.generate (A.constant (Z :. fbH :. fbW)) $ \ix ->
        let Z :. y :. x = A.unlift ix :: Z :. Exp Int :. Exp Int
            px = A.fromIntegral x :: Exp Float
            py = A.fromIntegral y :: Exp Float
            tileId = (y `A.quot` A.constant tileH) * A.constant tilesX + (x `A.quot` A.constant tileW)
            tileCount =
              A.min
                (A.constant maxPrimsPerTile)
                (A.fromIntegral (tileCounts A.! A.index1 tileId) :: Exp Int)
            initial = A.lift (tileCount - 1, A.constant 0xFF000000 :: Exp Word32)
            wcond st = let (i, _) = A.unlift st :: (Exp Int, Exp Word32) in i A.>= 0
            body st =
              let (i, acc) = A.unlift st :: (Exp Int, Exp Word32)
                  binIndex = tileId * A.constant maxPrimsPerTile + i
                  primIdx = A.fromIntegral (tileBinsFlat A.! A.index1 binIndex)
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
