module Main
where

import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.ZMQ3 as PZ
import qualified System.ZMQ3 as Z

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)

main :: IO ()
main = do
    -- create and use exactly one context in a single process
    Z.withContext $ \ctx ->
        Z.withSocket ctx Z.Rep $ \echoServer ->
        Z.withSocket ctx Z.Req $ \client -> do
            
            Z.bind echoServer "inproc://echoserver"
            forkIO $ echo echoServer
            putStrLn "Started echo server"

            Z.connect client "inproc://echoserver"
            putStrLn "The client will send stdout to the echoserver and print it back"
            runEffect $ PB.stdin >-> PZ.request client >-> PB.stdout
    where
        echo s =
            forever $ do
                msg <- Z.receive s
                -- Simulate doing some 'work' for 1 second
                threadDelay (1000000)
                Z.send s [] msg

