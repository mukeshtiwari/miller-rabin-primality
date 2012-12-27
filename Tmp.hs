{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally)

import System.IO
import System.Random

import Text.Printf


workerThread :: TVar Double -> TVar Int -> TMVar () -> IO ()
workerThread sumtv taskstv donemv = do
  rval <- randomIO :: IO Double
  case rval of
    _ | rval < 0.10 -> spawnWorker sumtv taskstv donemv >> spawnWorker sumtv taskstv donemv -- Spawn two threads
      | rval < 0.30 -> forever $ threadDelay 1000000 -- Sleep forever.
      | otherwise -> atomically $ modifyTVar' sumtv (+ rval)


spawnWorker :: TVar Double -> TVar Int -> TMVar () -> IO ()
spawnWorker sumtv taskstv donemv = void $ forkIO ((incTasks >> workerThread sumtv taskstv donemv) `finally` decTasks)
  where
    incTasks = atomically $ modifyTVar' taskstv (+ 1)
    decTasks = atomically $ do
      tasks <- readTVar taskstv
      let !newtasks = tasks - 1
      writeTVar taskstv newtasks
      when (newtasks < 1) . void $ tryPutTMVar donemv ()

main :: IO ()
main = do
  sumtv <- newTVarIO (0.0 :: Double)
  taskstv <- newTVarIO (0 :: Int)
  donemv <- newEmptyTMVarIO :: IO (TMVar ())
  replicateM_ 100000 $ spawnWorker sumtv taskstv donemv
  forever $ do
    done <- atomically $ tryTakeTMVar donemv
    tasks <- atomically $ readTVar taskstv
    sum <- atomically $ readTVar sumtv
    printf "done=%s\ttasks=%08d\tsum=%.8f\n" (show $ done /= Nothing) tasks sum
    threadDelay 100000
