{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Control.Lens
import qualified Data.Map as M
import qualified Data.Aeson as A
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import Data.Text (unpack)
import qualified Control.Monad.State as S
import qualified Data.Vector as V
import Data.Maybe

import Data.Char (toLower)

type Resp = Response BLI.ByteString
type JBody = M.Map String A.Value

type Id = String
type Result = String
type Session = (Int, BI.ByteString)
type CrawlState = ([Id], Session)


-- compatible with S.StateT queryApi of type S.StateT ([Id], Session) IO [Result]
queryApi :: ([Id], Session) -> IO (Maybe Result, ([Id], Session))
queryApi ([], session) = return (Nothing, ([], session))
queryApi (currentId:nextIds, session) = do
  (body, newSess) <- S.liftIO $ fetchNext currentId session
  let nextTargets = extractIds body
  let res = extractResult body
  return (res, (nextTargets ++ nextIds, newSess))

gather :: S.StateT CrawlState IO (Maybe Result) -> CrawlState
  -> IO ([Maybe Result], CrawlState)
gather mState s = do
  (res, newState@(ids, _)) <- S.runStateT mState s
  case ids of
    [] -> return ([res], newState)
    _ -> do
      (nextResults, nextState) <- gather mState newState
      return (res : nextResults, nextState)

main :: IO ()
main = do
  initialSession <- newSession
  let initialState = (["start"], initialSession)
  (results, _) <- gather (S.StateT queryApi) initialState
  putStrLn $ show $ concat . catMaybes $ results

root :: String
root = "http://challenge.shopcurbside.com/"
startUrl :: String
startUrl = root ++ "start"
sessionUrl :: String
sessionUrl = root ++ "get-session"

newSession :: IO Session
newSession = do
  r <- get sessionUrl
  return (10, BL.toStrict $ r ^. responseBody)

getWithSession :: BI.ByteString -> String -> IO Resp
getWithSession sess url = let opts = defaults & header "Session" .~ [sess]
  in getWith opts url

vectorToString :: V.Vector A.Value -> [String]
vectorToString v
  | v == V.empty = []
  | otherwise =
  case V.head v of A.String s -> unpack s : vectorToString (V.tail v)
                   _ -> vectorToString (V.tail v)

fetchNext :: String -> Session -> IO (JBody, Session)
fetchNext next (count, session)
    | count <= 0 = newSession >>= fetchNext next
    | otherwise = do
      resp <- asJSON =<< getWithSession session (root ++ next)
      -- S.liftIO $ putStrLn (show $ resp ^. responseBody)
      let body = toLowerKeys (resp ^. responseBody)
      return (body, (count - 1, session))

-- convert keys of the given map to lowercase equivalent
toLowerKeys :: M.Map String A.Value -> M.Map String A.Value
toLowerKeys = M.mapKeys (map toLower)
--
extractIds :: JBody -> [String]
extractIds m =
  case M.lookup "next" m of
    (Just (A.Array as)) -> vectorToString as
    (Just (A.String s)) -> [unpack s]
    _ -> []

extractResult :: JBody -> Maybe String
extractResult m =
  case M.lookup "secret" m of
    (Just (A.String s)) -> Just (unpack s)
    _ -> Nothing
