module Blitz.Draw
  ( DrawM,
    runDrawFrame,
    drawCircle,
    drawLine,
    Camera2D (..),
    withCamera,
  )
where

import Blitz.Framebuffer qualified as Blitz
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Int (Int32)
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word32)

data Camera2D = Camera2D
  { camCenterX :: Float,
    camCenterY :: Float,
    camZoom :: Float
  }

data Space
  = ScreenSpace
  | WorldSpace !Camera2D

baseScale :: Float
baseScale = 80.0

data DrawEnv = DrawEnv
  { tags :: VSM.IOVector Int32,
    x1s :: VSM.IOVector Float,
    y1s :: VSM.IOVector Float,
    x2s :: VSM.IOVector Float,
    y2s :: VSM.IOVector Float,
    sizes :: VSM.IOVector Float,
    colors :: VSM.IOVector Word32,
    maxPrims :: Int,
    circleTag :: Int32,
    lineTag :: Int32,
    space :: Space
  }

newtype DrawM a = DrawM {unDrawM :: DrawEnv -> Int -> IO (Int, a)}

instance Functor DrawM where
  {-# INLINE fmap #-}
  fmap f (DrawM m) = DrawM $ \env s -> do
    (s', a) <- m env s
    pure (s', f a)

instance Applicative DrawM where
  {-# INLINE pure #-}
  pure a = DrawM $ \_ s -> pure (s, a)
  {-# INLINE (<*>) #-}
  DrawM mf <*> DrawM ma = DrawM $ \env s -> do
    (s', f) <- mf env s
    (s'', a) <- ma env s'
    pure (s'', f a)

instance Monad DrawM where
  {-# INLINE (>>=) #-}
  DrawM ma >>= f = DrawM $ \env s -> do
    (s', a) <- ma env s
    let DrawM mb = f a
    mb env s'

instance MonadIO DrawM where
  {-# INLINE liftIO #-}
  liftIO io = DrawM $ \_ s -> do
    a <- io
    pure (s, a)

-- | Run a frame's worth of drawing. Resets index to 0, runs the action,
-- pads remaining slots with disabled primitives, returns final primitive count.
runDrawFrame ::
  VSM.IOVector Int32 ->
  VSM.IOVector Float ->
  VSM.IOVector Float ->
  VSM.IOVector Float ->
  VSM.IOVector Float ->
  VSM.IOVector Float ->
  VSM.IOVector Word32 ->
  Int ->
  Int32 ->
  Int32 ->
  DrawM a ->
  IO Int
runDrawFrame !tagsV !x1sV !y1sV !x2sV !y2sV !sizesV !colorsV !maxNumPrims !circleTagVal !lineTagVal action = do
  let env =
        DrawEnv
          { tags = tagsV,
            x1s = x1sV,
            y1s = y1sV,
            x2s = x2sV,
            y2s = y2sV,
            sizes = sizesV,
            colors = colorsV,
            maxPrims = maxNumPrims,
            circleTag = circleTagVal,
            lineTag = lineTagVal,
            space = ScreenSpace
          }
  let DrawM m = action
  (finalIdx, _) <- m env 0
  let !n = min finalIdx maxNumPrims
  -- Pad remaining slots with disabled primitives
  padDisabled env n maxNumPrims
  pure n

-- | Pad slots [start..end-1] with disabled primitives that never hit
{-# INLINE padDisabled #-}
padDisabled :: DrawEnv -> Int -> Int -> IO ()
padDisabled !env !start !end = go start
  where
    go !i
      | i >= end = pure ()
      | otherwise = do
          VSM.unsafeWrite env.tags i env.circleTag
          VSM.unsafeWrite env.x1s i (-1e9)
          VSM.unsafeWrite env.y1s i (-1e9)
          VSM.unsafeWrite env.x2s i 0
          VSM.unsafeWrite env.y2s i 0
          VSM.unsafeWrite env.sizes i 0
          VSM.unsafeWrite env.colors i 0x00000000
          go (i + 1)

-- | Run a drawing action in world space using the provided camera.
{-# INLINE withCamera #-}
withCamera :: Camera2D -> DrawM a -> DrawM a
withCamera !cam (DrawM m) = DrawM $ \env s ->
  m env {space = WorldSpace cam} s

{-# INLINE toPxPoint #-}
toPxPoint :: Space -> Float -> Float -> (Float, Float)
toPxPoint ScreenSpace !x !y = (x, y)
toPxPoint (WorldSpace cam) !wx !wy =
  let !screenCenterX = Prelude.fromIntegral Blitz.fbW * 0.5
      !screenCenterY = Prelude.fromIntegral Blitz.fbH * 0.5
      !scale = baseScale * cam.camZoom
      !sx = (wx - cam.camCenterX) * scale + screenCenterX
      !sy = (wy - cam.camCenterY) * scale + screenCenterY
   in (sx, sy)

{-# INLINE toPxLen #-}
toPxLen :: Space -> Float -> Float
toPxLen ScreenSpace !len = len
toPxLen (WorldSpace cam) !len =
  let !scale = baseScale * cam.camZoom
   in len * scale

-- Colors use ABGR (0xAABBGGRR) so bytes are RGBA on little-endian.
premulABGR :: Word32 -> Word32
premulABGR c =
  let a = (c `shiftR` 24) .&. 0xFF
      b = (c `shiftR` 16) .&. 0xFF
      g = (c `shiftR` 8) .&. 0xFF
      r = c .&. 0xFF
      mul x = (x * a + 127) `quot` 255
      r' = mul r
      g' = mul g
      b' = mul b
   in (a `shiftL` 24) .|. (b' `shiftL` 16) .|. (g' `shiftL` 8) .|. r'

-- | Draw a circle at (x, y) with radius r and color col (ABGR).
{-# INLINE drawCircle #-}
drawCircle :: Float -> Float -> Float -> Word32 -> DrawM ()
drawCircle !x !y !r !col = DrawM $ \env !i ->
  if i < env.maxPrims
    then do
      let (!px, !py) = toPxPoint env.space x y
          !pr = toPxLen env.space r
          !pcol = premulABGR col
      VSM.unsafeWrite env.tags i env.circleTag
      VSM.unsafeWrite env.x1s i px
      VSM.unsafeWrite env.y1s i py
      VSM.unsafeWrite env.x2s i 0
      VSM.unsafeWrite env.y2s i 0
      VSM.unsafeWrite env.sizes i pr
      VSM.unsafeWrite env.colors i pcol
      let !i' = i + 1
      pure (i', ())
    else pure (i, ())

-- | Draw a line from (x1, y1) to (x2, y2) with thickness and color (ABGR).
{-# INLINE drawLine #-}
drawLine :: Float -> Float -> Float -> Float -> Float -> Word32 -> DrawM ()
drawLine !lx1 !ly1 !lx2 !ly2 !thickness !col = DrawM $ \env !i ->
  if i < env.maxPrims
    then do
      let (!px1, !py1) = toPxPoint env.space lx1 ly1
          (!px2, !py2) = toPxPoint env.space lx2 ly2
          !pth = toPxLen env.space thickness
          !pcol = premulABGR col
      VSM.unsafeWrite env.tags i env.lineTag
      VSM.unsafeWrite env.x1s i px1
      VSM.unsafeWrite env.y1s i py1
      VSM.unsafeWrite env.x2s i px2
      VSM.unsafeWrite env.y2s i py2
      VSM.unsafeWrite env.sizes i pth
      VSM.unsafeWrite env.colors i pcol
      let !i' = i + 1
      pure (i', ())
    else pure (i, ())
