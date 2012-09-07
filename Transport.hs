import Network.Transport
import Network.Transport.TCP ( createTransport , defaultTCPParameters ) 
import Control.Concurrent
import Control.Monad
import Data.String

main = do 
  serverAddr <- newEmptyMVar
  clientAddr <- newEmptyMVar
  done <- newEmptyMVar 

  Right transport <- createTransport "127.0.0.1" "10080" defaultTCPParameters

  --"server" 
  forkIO $ do 
         Right endpoint <-   newEndPoint transport 
         putMVar serverAddr ( address endpoint ) 
       
         Right conn <- do addr <- readMVar clientAddr 
                          connect endpoint addr ReliableOrdered defaultConnectHints
               
         forever $ do 
            event <- receive endpoint 
            case event of 
               Received _ msg -> do 
                                    putStrLn "I am server and  received message from client"
                                    print msg
                                    return ()
               _ -> return ()
            threadDelay 1000000
            send conn [ fromString "hello client" ]
            

  --"client"
  forkIO $ do 
       Right endpoint <- newEndPoint transport 
       putMVar clientAddr ( address endpoint )

       Right conn <- do addr <- readMVar serverAddr 
                        connect endpoint addr ReliableOrdered defaultConnectHints

       forever $ do 
            event <- receive endpoint
            case event of 
              Received _ msg -> do 
                                  putStrLn "I am client and  received message from server"
                                  print msg
                                  return ()
              _ -> return ()
            send conn [ fromString "hello server" ]
       
       putMVar done ()
            
            
  takeMVar done
