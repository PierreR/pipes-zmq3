{-# LANGUAGE RankNTypes #-}

module Pipes.ZMQ3 (
	fromZMQ
    ) where

import qualified Data.ByteString as B

import Pipes
import qualified System.ZMQ3 as Z
import Control.Monad (forever)


{-| Send upstream bytes into a request socket, 
    wait/block for the reply,
    yield the reply
-}
request :: MonadIO m => Z.Socket Z.Req -> Pipe B.ByteString B.ByteString m ()
request sock = forever $ do
    await >>= liftIO . Z.send sock []
    fromZMQ sock

-- | Use a Receiver Socket to produce 'ByteString's
fromZMQ :: (MonadIO m, Z.Receiver t)  => Z.Socket t -> Producer' B.ByteString m ()
fromZMQ sock  = forever $
	liftIO (Z.receive sock) >>= yield