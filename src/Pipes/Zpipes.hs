{-# LANGUAGE OverloadedStrings #-}
module Pipes.Zpipes
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL

import Pipes
--import Pipes.Core(Client, request)
import Pipes.ByteString as PB
--import qualified Pipes.Prelude as P
import qualified System.ZMQ3 as Z

{-| Send some bytes over ZMQ, and 
    wait for the reply
-}
--sendAndReceive :: (Z.Sender r, Z.Receiver r) => Z.Socket z r -> Pipe B.ByteString B.ByteString (ZMQ z) ()
--sendAndReceive sock = do
--    for cat $ \x -> do
--        lift $ Z.send sock [] x 
--        (lift $ Z.receive sock) >>= yield

request :: (Z.Sender t, Z.Receiver t) => Z.Socket t => Pipe B.ByteString B.ByteString IO ()
request = loop
	where
		loop sock = do
			await >>= lift . Z.send sock []
			(lift $ Z.receive sock) >>= yield
		 	loop sock

--receive :: (Z.Receiver t) => Z.Socket t => Producer B.ByteString IO r
--receive = loop
--	where
--		loop sock = do
--			(lift $ Z.receive sock) >>= yield
--			loop sock

main :: IO ()
main = do
    Z.withContext $ \ctx ->
    	Z.withSocket ctx Z.Req $ \reqSock -> do
    		Z.connect reqSock "tcp://localhost:5555"
        	--lift $ putStrLn "Started echo client"
        	runEffect $ PB.stdin >-> request reqSock >-> PB.stdout -- >-> receiveZ reqSock >-> hoist liftIO P.stdout