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
import Control.Applicative((<$>), (<*>))

import qualified Control.Foldl as Left

import Data.ByteString.Char8 (pack, unpack)
import Text.Printf
import System.Random (randomRIO)

pubServerThread :: Z.Sender t => Z.Socket t -> IO r
pubServerThread s = forever $ do
    threadDelay (25) -- be gentle with the CPU
    zipcode <- randomRIO (10000::Int, 11000)
    temperature <- randomRIO (-10::Int, 35)
    humidity <- randomRIO (10::Int, 60)
    let update = pack $ unwords [show zipcode, show temperature, show humidity]
    Z.send s [] update

fold' :: (Monad m )=> Left.Fold a b -> Producer a m () -> m b
fold' myFold = case myFold of
    Left.Fold step begin done -> P.fold step begin done

-- | This function will be part of foldl later on
--   @fold (mapped f folder) list == fold folder (map f list)@
mapped :: (a -> b) -> Left.Fold b r -> Left.Fold a r
mapped f (Left.Fold step begin done) = Left.Fold step' begin done
  where
    step' x = step x . f

average :: Left.Fold Int Int
average = div <$> Left.sum <*> Left.length


main :: IO ()
main = do
    -- create and use exactly one context in a single process
    Z.withContext $ \ctx ->
        Z.withSocket ctx Z.Pub $ \pubSocket ->
        Z.withSocket ctx Z.Sub $ \subSocket -> do
            
            Z.bind pubSocket "inproc://pubserver"
            putStrLn "Starting pub server"
            async $ pubServerThread pubSocket

            Z.connect subSocket "inproc://pubserver"
            Z.subscribe subSocket (pack "10001")

            evalStateT reporter (processedData subSocket)
    where 

        reporter :: StateT (Producer (Int, Int, Int) IO  r) IO ()
        reporter = loop
            where 
                loop = do
                    (avgTemp, avgHum) <- fold' averages (input >-> P.take 10)
                    liftIO $ printf "-- Report: average temperature is %d°C, average humidity is %d%% \n" avgTemp avgHum   
                    eof <- isEndOfInput
                    unless eof loop

        averages :: Left.Fold (Int, Int, Int) (Int, Int)
        averages =
            let avgTemp :: Left.Fold (Int, Int, Int) Int
                avgTemp = mapped (\(_, t, _) -> t) average

                avgHumidity :: Left.Fold (Int, Int, Int) Int
                avgHumidity = mapped (\(_, _, h) -> h) average

            in  (,) <$> avgTemp <*> avgHumidity

        processedData :: Z.Socket Z.Sub -> Producer (Int, Int, Int) IO ()
        processedData subSocket = for (PZ.fromSub subSocket) $ \bs -> do
            let [zipcode, temperature, humidity] = map read $ words (unpack bs)
            liftIO $ printf "At NY City (%d), temperature of %d and humidity %d\n"  zipcode temperature humidity          
            yield (zipcode, temperature, humidity)

            



