module PingParser where

import Text.ParserCombinators.Parsec
import Text.Parsec.Char (endOfLine)
import Numeric (readFloat)
import Control.Applicative (empty, (<$), (<$>), (<*>), (<*))
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Maybe (catMaybes)

data Ping = Ping {icmpSeq :: Int, time :: Float} deriving (Show)

-- utils to parse int and float
positiveFloat :: CharParser () Float
positiveFloat = do
  s <- getInput
  case readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

positiveNatural :: CharParser () Int
positiveNatural =
  foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit

pingLine :: CharParser () (Maybe Ping)
pingLine = pingHeader <|> (host >> pingData)

pingHeader :: CharParser () (Maybe Ping)
pingHeader = string "PING" >> many (noneOf "\n") >> return Nothing

host :: CharParser () String
host = many (noneOf ":") <* char ':' <* spaces

-- fancy applicative version
pingData :: CharParser () (Maybe Ping)
pingData = Just <$> (
    Ping <$> (pair positiveNatural <* spaces)
    -- discard the ttl and the remaining " ms" at the end of the line
    <*> between (pair positiveNatural) (string " ms") (pair positiveFloat)
  )

-- -- same as above with a do notation
-- pingData = do
--   icmp <- pair positiveNatural
--   spaces
--   _ <- pair positiveNatural -- ttl
--   spaces
--   time <- pair positiveFloat
--   spaces >>  string "ms"
--   return Ping {icmpSeq=icmp, time=time}


pair :: CharParser () a -> CharParser () a
pair parser = many (noneOf "=") >> char '=' >> parser

pingFile :: CharParser () [Maybe Ping]
pingFile = sepEndBy pingLine endOfLine

parsePingFile :: String -> Either ParseError [Ping]
parsePingFile input = case parse pingFile "(unknown)" input of
  Left err -> Left err
  Right pings -> Right $ catMaybes pings
