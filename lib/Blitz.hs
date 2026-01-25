{-# LANGUAGE BangPatterns #-}

module Blitz where

import Control.Concurrent (forkOS)
import Control.Monad (unless, void)
import Data.Bits (shiftL, (.|.))
import Data.IORef (IORef, atomicWriteIORef, modifyIORef', newIORef, readIORef)
import Data.Word (Word32, Word8)
import Foreign.Marshal.Array (mallocArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (pokeByteOff)
import Foreign.Store (Store (..))
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util.Colors

type TickAction = IO ()

{-# NOINLINE tickStore #-}
tickStore :: Store (IORef TickAction)
tickStore = Store 0

windowWidth, windowHeight :: Int
windowWidth = 1920
windowHeight = 1200

targetFramesPerSecond :: Int
targetFramesPerSecond = 60

fbW, fbH :: Int
fbW = 320
fbH = 180

rgba :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
rgba r g b a =
  (fromIntegral r `shiftL` 0)
    .|. (fromIntegral g `shiftL` 8)
    .|. (fromIntegral b `shiftL` 16)
    .|. (fromIntegral a `shiftL` 24)

fillPixels :: Ptr Word32 -> Int -> Int -> Int -> IO ()
fillPixels p w h frame = go 0 0 0
  where
    !n = w * h
    go !i !x !y
      | i >= n = pure ()
      | otherwise = do
          let r = fromIntegral ((x + frame) `mod` 256)
              g = fromIntegral ((y + frame) `mod` 256)
              b = fromIntegral ((x + y) `mod` 256)
              px = rgba r g b 255
          pokeByteOff p (i * 4) px
          let x' = x + 1
          if x' == w
            then go (i + 1) 0 (y + 1)
            else go (i + 1) x' y

main :: IO ()
main = do
  tickRef <- newIORef (pure ())
  runWindow tickRef

mainDev :: IO ()
mainDev = do
  let Store storeId = tickStore
  existing <- FS.lookupStore storeId
  case existing of
    Nothing -> do
      putStrLn "booting"
      tickRef <- newIORef (pure ())
      FS.writeStore tickStore tickRef
      void $ forkOS $ do
        runWindow tickRef
        FS.deleteStore tickStore
        putStrLn "shutting down"
    Just _ -> do
      putStrLn "reloading"
      tickRef <- FS.readStore tickStore
      atomicWriteIORef tickRef (pure ())

runWindow :: IORef TickAction -> IO ()
runWindow tickRef = do
  window <- initWindow windowWidth windowHeight "blitz"
  setTargetFPS targetFramesPerSecond

  img <- genImageColor fbW fbH black
  tex <- loadTextureFromImage img
  _ <- setTextureFilter tex TextureFilterPoint

  pixels <- mallocArray (fbW * fbH) :: IO (Ptr Word32)

  frameRef <- newIORef (0 :: Int)

  atomicWriteIORef tickRef $ do
    frame <- readIORef frameRef
    modifyIORef' frameRef (+ 1)

    fillPixels pixels fbW fbH frame

    updateTexture tex (castPtr pixels)

    sw <- getScreenWidth
    sh <- getScreenHeight
    beginDrawing
    clearBackground black

    let src = Rectangle 0 0 (fromIntegral fbW) (fromIntegral fbH)
        dst = Rectangle 0 0 (fromIntegral sw) (fromIntegral sh)
        origin = Vector2 0 0
    drawTexturePro tex src dst origin 0 white

    endDrawing

  gameLoop tickRef

  unloadTexture tex window
  closeWindow (Just window)

gameLoop :: IORef TickAction -> IO ()
gameLoop tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn
    gameLoop tickRef
