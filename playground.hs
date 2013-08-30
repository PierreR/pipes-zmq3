module Main
where

import Pipes
import qualified Pipes.ByteString as PB
import qualified Pipes.Zpipes as PZ
import qualified System.ZMQ3 as Z

main :: IO ()
main = do
    Z.withContext $ \ctx ->
    	Z.withSocket ctx Z.Req $ \reqSock -> do
    		Z.connect reqSock "tcp://localhost:5555"
        	putStrLn "Started echo client"
        	runEffect $ PB.stdin >-> PZ.request reqSock >-> PB.stdout