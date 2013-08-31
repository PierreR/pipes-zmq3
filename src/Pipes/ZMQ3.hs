module Pipes.ZMQ3
where

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
    (liftIO $ Z.receive sock) >>= yield

-- | Use a Subscription Socket to produce 'ByteString's
fromSub :: MonadIO m => Z.Socket Z.Sub -> Producer B.ByteString m ()
fromSub sock  = forever $ do
	(liftIO $ Z.receive sock) >>= yield