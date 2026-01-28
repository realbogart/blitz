module Blitz.Draw
  ( DrawM,
    runDrawFrame,
    drawCircle,
    drawLine,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word32)

data DrawEnv = DrawEnv
  { tags :: VSM.IOVector Int32,
    x1s :: VSM.IOVector Float,
    y1s :: VSM.IOVector Float,
    x2s :: VSM.IOVector Float,
    y2s :: VSM.IOVector Float,
    sizes :: VSM.IOVector Float,
    colors :: VSM.IOVector Word32,
    circleTag :: Int32,
    lineTag :: Int32
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
runDrawFrame !tagsV !x1sV !y1sV !x2sV !y2sV !sizesV !colorsV !numPrims !circleTagVal !lineTagVal action = do
  let env =
        DrawEnv
          { tags = tagsV,
            x1s = x1sV,
            y1s = y1sV,
            x2s = x2sV,
            y2s = y2sV,
            sizes = sizesV,
            colors = colorsV,
            circleTag = circleTagVal,
            lineTag = lineTagVal
          }
  let DrawM m = action
  (finalIdx, _) <- m env 0
  let !n = min finalIdx numPrims
  -- Pad remaining slots with disabled primitives
  padDisabled env n numPrims
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

-- | Draw a circle at (x, y) with radius r and color col
{-# INLINE drawCircle #-}
drawCircle :: Float -> Float -> Float -> Word32 -> DrawM ()
drawCircle !x !y !r !col = DrawM $ \env !i -> do
  VSM.unsafeWrite env.tags i env.circleTag
  VSM.unsafeWrite env.x1s i x
  VSM.unsafeWrite env.y1s i y
  VSM.unsafeWrite env.x2s i 0
  VSM.unsafeWrite env.y2s i 0
  VSM.unsafeWrite env.sizes i r
  VSM.unsafeWrite env.colors i col
  let !i' = i + 1
  pure (i', ())

-- | Draw a line from (x1, y1) to (x2, y2) with thickness and color
{-# INLINE drawLine #-}
drawLine :: Float -> Float -> Float -> Float -> Float -> Word32 -> DrawM ()
drawLine !lx1 !ly1 !lx2 !ly2 !thickness !col = DrawM $ \env !i -> do
  VSM.unsafeWrite env.tags i env.lineTag
  VSM.unsafeWrite env.x1s i lx1
  VSM.unsafeWrite env.y1s i ly1
  VSM.unsafeWrite env.x2s i lx2
  VSM.unsafeWrite env.y2s i ly2
  VSM.unsafeWrite env.sizes i thickness
  VSM.unsafeWrite env.colors i col
  let !i' = i + 1
  pure (i', ())
