import PingParser
import Data.Maybe
import Data.List

computeDropped :: [Ping] -> Int
computeDropped pings = let
  folder (count, prev) ping
    | icmp == (prev + 1) = (count, icmp)
    | icmp < prev = (count, icmp) -- new ping test
    | otherwise = (count + icmp - prev - 1, icmp)
    where icmp = icmpSeq ping
  in fst $ foldl' folder (0, 0) pings

data PingStats = PingStats {
  mean :: Float,
  minTime :: Float,
  firstQuartile :: Float,
  median :: Float,
  thirdQuartile :: Float,
  maxTime :: Float
}

pingStats :: [Ping] -> PingStats
pingStats pings = let
  folder (minTime, maxTime, total, count) Ping {icmpSeq=icmp, time=time} =
    (
      min minTime time,
      max maxTime time,
      total + time,
      count + 1
    )
  (minTime, maxTime, total, count) = foldl' folder (0, 0, 0, 0) pings
  in PingStats {minTime=minTime, maxTime=maxTime, mean=total/count, firstQuartile=0, thirdQuartile=0, median=0}

main :: IO ()
main = do
  content <- readFile "test.dat"
  let parsed = parsePingFile content
  case parsePingFile content of
    Left err -> putStrLn $ "ERROR: " ++ (show err)
    Right stuff -> putStrLn $ show $ computeDropped stuff

getEither :: () -> Either String (Maybe String)
getEither _ = Right $ Just "right string"

getJust :: (Maybe String) -> String
getJust (Just s) = s
getJust _ = "nothing"

testMonads :: Either String String -> String
testMonads input = do
  r <- getEither
  do j <- getJust r
    "foo"
