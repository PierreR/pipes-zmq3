{-# LANGUAGE OverloadedStrings #-}
module Main
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Pipes
--import Pipes.Core(Client, request)
import Pipes.ByteString as PB
--import qualified Pipes.Prelude as P
import qualified Pipes.Zpipes as Z
import qualified System.ZMQ3 as Z

main :: IO ()
main = do
    Z.withContext $ \ctx ->
    	Z.withSocket ctx Z.Req $ \reqSock -> do
    		Z.connect reqSock "tcp://localhost:5555"
        	--lift $ putStrLn "Started echo client"
        	runEffect $ PB.stdin >-> Z.request reqSock >-> PB.stdout -- >-> receiveZ reqSock >-> hoist liftIO P.stdout