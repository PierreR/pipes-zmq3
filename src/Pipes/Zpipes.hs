module Pipes.Zpipes
where

import qualified Data.ByteString as B

import Pipes
import qualified System.ZMQ3 as Z


{-| Send some bytes over ZMQ, and 
    wait for the reply
-}
request :: (Z.Sender t, Z.Receiver t) => Z.Socket t => Pipe B.ByteString B.ByteString IO ()
request = loop
    where
        loop sock = do
            await >>= lift . Z.send sock []
            (lift $ Z.receive sock) >>= yield
            loop sock
