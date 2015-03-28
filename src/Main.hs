module Main where

import System.Environment
import PingParser
import Data.Maybe
import Data.List

getFileName :: [String] -> String
getFileName [] = "./googlePing.dat"
getFileName (file:_) = file

-- analysePings :: [Ping] -> String
-- analysePings [] = ""

meanTime :: [Ping] -> Float
meanTime pings = let (total, count) = (foldl' (\(total, count) ping -> (total + time ping, count +1)) (0, 0) pings)
  in total/count

countDropped :: [Ping] -> Int
countDropped pings = let
  folder (count, prev) ping
    | icmp == (prev + 1) = (count, icmp)
    | icmp < prev = (count, icmp) -- new ping test
    | otherwise = (count + icmp - prev - 1, icmp)
    where icmp = icmpSeq ping
  in fst $ foldl' folder (0, 0) pings

printDropped :: [Ping] -> String
printDropped pings = let
  count = countDropped pings
  total = length pings
  in (show count) ++ "/" ++ (show total) ++ " dropped ping."

main :: IO ()
main = do
  args <- getArgs
  let filePath = getFileName args
  putStrLn ("going to process " ++ filePath)
  rawPingData <- readFile filePath
  let pings = parsePingFile rawPingData
  case pings of
    Left err -> putStrLn $ show err
    Right stuff -> putStrLn $ printDropped stuff
