module Main
where

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ZMQ3 as PZ
import qualified System.ZMQ3 as Z

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.ByteString.Char8 (pack, unpack)
import System.Random (randomRIO)



main :: IO ()
main = do
    -- create and use exactly one context in a single process
    Z.withContext $ \ctx ->
        Z.withSocket ctx Z.Pub $ \pubSocket ->
        Z.withSocket ctx Z.Sub $ \subSocket -> do
            
            Z.bind pubSocket "inproc://pubserver"
            putStrLn "Starting pub server"
            forkIO $ pubServer pubSocket

            Z.connect subSocket "inproc://pubserver"
            Z.subscribe subSocket (pack "10001")
            runEffect $ PZ.fromSub subSocket >-> P.map (display.words.unpack) >->  P.stdout
    where
        
        pubServer s = forever $ do
            --threadDelay (10) 
            zipcode <- randomRIO (0::Int, 100000)
            temperature <- randomRIO (-80::Int, 135)
            humidity <- randomRIO (10::Int, 60)
            let update = pack $ unwords [show zipcode, show temperature, show humidity]
            Z.send s [] update
        
        display:: [String] -> String
        display [_, temp, hum] =
            unwords ["New York", "Temperature:" , temp, "Humidity:" , hum] 
