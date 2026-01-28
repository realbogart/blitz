{-# LANGUAGE BangPatterns #-}

module Blitz.Draw
  ( DrawM,
    runDrawFrame,
    drawCircle,
    drawLine,
  )
where

import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State.Strict
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
    maxPrims :: Int,
    circleTag :: Int32,
    lineTag :: Int32
  }

newtype DrawM a = DrawM {unDrawM :: ReaderT DrawEnv (StateT Int IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

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
            maxPrims = numPrims,
            circleTag = circleTagVal,
            lineTag = lineTagVal
          }
  let DrawM m = action
  !finalIdx <- execStateT (runReaderT m env) 0
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
drawCircle !x !y !r !col = DrawM $ do
  env <- ask
  !i <- get
  when (i < env.maxPrims) $ do
    liftIO $ do
      VSM.unsafeWrite env.tags i env.circleTag
      VSM.unsafeWrite env.x1s i x
      VSM.unsafeWrite env.y1s i y
      VSM.unsafeWrite env.x2s i 0
      VSM.unsafeWrite env.y2s i 0
      VSM.unsafeWrite env.sizes i r
      VSM.unsafeWrite env.colors i col
    put $! i + 1

-- | Draw a line from (x1, y1) to (x2, y2) with thickness and color
{-# INLINE drawLine #-}
drawLine :: Float -> Float -> Float -> Float -> Float -> Word32 -> DrawM ()
drawLine !lx1 !ly1 !lx2 !ly2 !thickness !col = DrawM $ do
  env <- ask
  !i <- get
  when (i < env.maxPrims) $ do
    liftIO $ do
      VSM.unsafeWrite env.tags i env.lineTag
      VSM.unsafeWrite env.x1s i lx1
      VSM.unsafeWrite env.y1s i ly1
      VSM.unsafeWrite env.x2s i lx2
      VSM.unsafeWrite env.y2s i ly2
      VSM.unsafeWrite env.sizes i thickness
      VSM.unsafeWrite env.colors i col
    put $! i + 1
