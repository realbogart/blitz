module Blitz where

import Control.Concurrent (forkOS)
import Control.Monad (unless, void)
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef)
import Foreign.Store (Store (..))
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Util.Colors

type TickAction = IO ()

{-# NOINLINE tickStore #-}
tickStore :: Store (IORef TickAction)
tickStore = Store 0

windowWidth :: Int
windowWidth = 1920

windowHeight :: Int
windowHeight = 1200

targetFramesPerSecond :: Int
targetFramesPerSecond = 60

tick :: TickAction
tick = do
  beginDrawing
  clearBackground black
  endDrawing

main :: IO ()
main = do
  tickRef <- newIORef tick
  runWindow tickRef

mainDev :: IO ()
mainDev = do
  let Store storeId = tickStore
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

runWindow :: IORef TickAction -> IO ()
runWindow tickRef = do
  window <- initWindow windowWidth windowHeight "blitz"
  setTargetFPS targetFramesPerSecond
  gameLoop tickRef
  closeWindow (Just window)

gameLoop :: IORef TickAction -> IO ()
gameLoop tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn
    gameLoop tickRef
