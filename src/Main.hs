module Main where

import System.Environment
import Data.List
-- import System.IO
-- import qualified Data.ByteString.Lazy as BL

getFileName :: [String] -> String
getFileName [] = "./googlePing.dat"
getFileName (file:_) = file

-- Some lines have a different format like below
-- 64 bytes from 216.58.210.46: icmp_seq=133 ttl=58 time=115 ms
-- These are going to be ignored (at least in the first version of this
-- toy program)
isValidPing :: String -> Bool
isValidPing line | "64 bytes" `isPrefixOf` line = True
                 | otherwise = False

-- preprocessPingData :: ByteString -> [String]
-- pre

main :: IO ()
main = do
  args <- getArgs
  let filePath = getFileName args
  putStrLn ("going to process " ++ filePath)
  rawPingData <- readFile filePath
  let pingData = filter isValidPing $ lines rawPingData
  putStrLn ((show $ length pingData) ++ "lines to process")
