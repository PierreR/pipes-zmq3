module Main
where

import Pipes
import Pipes.Parse 
import qualified Pipes.Prelude as P
import qualified Pipes.ZMQ3 as PZ
import qualified System.ZMQ3 as Z

import Control.Concurrent(threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever, unless)

import Data.ByteString.Char8 (pack, unpack)
import Text.Printf
import System.Random (randomRIO)

main :: IO ()
main = do
    -- create and use exactly one context in a single process
    Z.withContext $ \ctx ->
        Z.withSocket ctx Z.Pub $ \pubSocket ->
        Z.withSocket ctx Z.Sub $ \subSocket -> do
            
            Z.bind pubSocket "inproc://pubserver"
            putStrLn "Starting pub server"
            async $ pubServer pubSocket
            Z.connect subSocket "inproc://pubserver"
            Z.subscribe subSocket (pack "10001")
            evalStateT loop (processedData subSocket)
    where 

        loop :: StateT (Producer (Int, Int, Int) IO  r) IO ()
        loop = do
            temp <- P.sum (input >-> P.take 10 >-> P.map (\(_, t, _) -> t))
            lift $ printf "-- Report: sum temp is %d --" temp
            eof <- isEndOfInput
            unless eof loop

        pubServer s = forever $ do
            threadDelay (28) -- be gentle with the CPU
            zipcode <- randomRIO (10000::Int, 11000)
            temperature <- randomRIO (-10::Int, 35)
            humidity <- randomRIO (10::Int, 60)
            let update = pack $ unwords [show zipcode, show temperature, show humidity]
            Z.send s [] update
        
        processedData :: Z.Socket Z.Sub -> Producer (Int, Int, Int) IO ()
        processedData subSocket = for (PZ.fromSub subSocket) $ \bs -> do
            let ws = words (unpack bs)
            liftIO $ putStrLn (display ws)
            let [zipcode, temperature, humidity] = map read ws
            yield (zipcode, temperature, humidity)

        display:: [String] -> String
        display [_, temp, hum] =
            unwords ["NY City", "Temperature:" , temp, "Humidity:" , hum]



