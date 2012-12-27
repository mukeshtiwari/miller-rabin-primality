import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Data.IORef


incr_count :: TVar Int -> TMVar () -> STM ()
incr_count m n = ( forM_ [ 1 .. 10000 ] $ \_ -> modifyTVar' m ( + 10 ) ) >> putTMVar n ()

main = do 
   count <- newTVarIO  0 
   list <- forM [1..10] $ \_ -> newEmptyTMVarIO 
   forM_ list $ \var -> forkIO . atomically . incr_count  count $ var 
   forM_ list $ \var ->  atomically . takeTMVar $ var    
   --threadDelay 1000000
   val <- readTVarIO count
   print val  

{--
incr_count :: MVar () -> MVar Int -> IO ()
incr_count m n = ( forM_ [ 1..10000 ] $ \_ -> modifyMVar_ n ( return . ( + 10 ) ) ) >> putMVar m ()

main :: IO()
main = do 
      count <- newMVar 0 
      list <- forM [1..10] $ \_ -> newEmptyMVar
      forM_ list $ \var -> forkIO . incr_count var $ count
      forM_ list $ \var ->  takeMVar var
      val <- takeMVar count
      print val
--}

{--

incr_count :: MVar () -> IORef Int  -> IO ()
incr_count m n = ( forM_ [ 1 .. 10000 ] $ \_ -> modifyIORef' n ( + 10   ))  >> putMVar m ()

main :: IO ()
main = do 
      count <- newIORef 0
      list <- forM [1..10] $ \_ -> newEmptyMVar
      forM_ list $ \var -> forkIO . incr_count var $ count
      forM_ list $ \var ->  takeMVar var          
      val <- readIORef count
      print val
--}

