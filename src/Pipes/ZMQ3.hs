module Pipes.ZMQ3
where

import qualified Data.ByteString as B

import Pipes
import qualified System.ZMQ3 as Z
import Control.Monad (forever)


{-| Send some bytes over ZMQ, and 
    wait for the reply
-}
request :: (Z.Sender t, Z.Receiver t) => Z.Socket t -> Pipe B.ByteString B.ByteString IO ()
request sock = forever $ do
    await >>= lift . Z.send sock []
    (lift $ Z.receive sock) >>= yield

fromSub :: (Z.Receiver t) => Z.Socket t -> Producer B.ByteString IO ()
fromSub sock  = forever $ do
	(lift $ Z.receive sock) >>= yield