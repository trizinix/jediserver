{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

import Prelude hiding ((!!))

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

import Debug.Trace
import Data.List.Safe ((!!))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Map as Map
import Data.ByteString.Char8 (unpack, ByteString)
import Data.ByteString.Lazy (append, fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Network.Wai as Wai
import Web.Frank
import Web.Simple.Responses (okHtml, okJson, notFound)
import Web.Simple.Controller (controllerApp)
import Web.Simple.Controller.Trans (respond, request, queryParam)
import System.Random
import Network.HTTP.Types
import Network.URL
import Network.WebSockets
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets


data HomeWorld = HomeWorld
  { world_id :: Int
  , world_name :: Text }
  deriving (Generic, Show)

instance ToJSON HomeWorld where
  toJSON (HomeWorld i n) = object [ "id" .= i, "name" .= n]

data Jedi = Jedi
  { id :: Int
  , name :: Text
  , homeworld :: HomeWorld
  , master :: Maybe Int
  , apprentice :: Maybe Int
  }
  deriving (Generic, ToJSON, Show)

darkJedis = [ Jedi 5105 "Xendor" (HomeWorld 58 "Coruscant") Nothing (Just 4629),
   Jedi 4629 "Ajunta Pall" (HomeWorld 19 "Alderaan") (Just 5105) (Just 4601),
   Jedi 4601 "Simus" (HomeWorld 27 "Korriban") (Just 4629) (Just 2950),
   Jedi 2950 "Naga Sadow" (HomeWorld 64 "Ziost") (Just 4601) (Just 8295),
   Jedi 8295 "Freedon Nadd" (HomeWorld 62 "Onderon") (Just 2950) (Just 2941),
   Jedi 2941 "Exar Kun" (HomeWorld 58 "Coruscant") (Just 8295) (Just 1548),
   Jedi 1548 "Jorak Uln" (HomeWorld 27 "Korriban") (Just 2941) (Just 4618),
   Jedi 4618 "Skere Kaan" (HomeWorld 58 "Coruscant") (Just 1548) (Just 5611),
   Jedi 5611 "Na'daz" (HomeWorld 35 "Ryloth") (Just 4618) (Just 9285),
   Jedi 9285 "Kas'im" (HomeWorld 38 "Nal Hutta") (Just 5611) (Just 3256),
   Jedi 3256 "Darth Bane" (HomeWorld 40 "Apatros") (Just 9285) (Just 3385),
   Jedi 3385 "Darth Zannah" (HomeWorld 11 "Somov Rit") (Just 3256) (Just 2942),
   Jedi 2942 "Darth Cognus" (HomeWorld 5 "Iktotch") (Just 3385) (Just 1121),
   Jedi 1121 "Darth Millennial" (HomeWorld 94 "Dromund Kaas") (Just 2942) (Just 5956),
   Jedi 5956 "Darth Tenebrous" (HomeWorld 90 "Clak'dor VII") (Just 1121) (Just 2350),
   Jedi 2350 "Darth Plagueis" (HomeWorld 83 "Mygeeto") (Just 5956) (Just 3616),
   Jedi 3616 "Darth Sidious" (HomeWorld 7 "Naboo") (Just 2350) (Just 1489),
   Jedi 1489 "Darth Vader" (HomeWorld 18 "Tatooine") (Just 3616) (Just 1330),
   Jedi 1330 "Antinnis Tremayne" (HomeWorld 58 "Coruscant") (Just 1489) Nothing
                ]

restPort = 3000

getBaseUrl :: ByteString -> Maybe String
getBaseUrl host = do
  url <- importURL $ unpack host
  let baseUrl = getHost (url_type url)
  exportHost `fmap` baseUrl

  where getHost (Absolute host) = Just host
        getHost _ = Nothing

randomJedi :: IO Jedi
randomJedi = do
  i <- randomRIO(1, length darkJedis)
  return $ fromJust (darkJedis !! (i-1))

randomDelay :: IO ()
randomDelay = do
      nsec <- randomRIO(500000, 1500000)
      threadDelay nsec

getJedi :: Maybe String -> Maybe Jedi
getJedi n = do
  k <- read `fmap` n
  (getById k) !! 0
    where getById n = filter (\j -> Main.id j == n) darkJedis

app :: Wai.Application
app = controllerApp () $ do
  get "/dark-jedis" $ do
    jedi <- liftIO $ do
      randomDelay
      randomJedi
    req <- request
    let headerHost = Wai.rawPathInfo req
    respond $ okJson $ encode jedi
  get "/dark-jedis/:id" $ do
    jediId <- queryParam "id"
    let jedi = getJedi jediId
    liftIO randomDelay
    traceM $ show jediId
    case jedi of
      Just jedi -> respond $ okJson $ encode jedi
      Nothing -> respond notFound


wsApp :: ServerApp
wsApp pending_conn = do
  conn <- acceptRequest pending_conn
  jedi <- liftIO randomJedi
  sendTextData conn (encode jedi)
  forever $ do
    jedi <- liftIO $ do
      putStrLn "Sende Jedi"
      randomDelay
      randomJedi
    sendTextData conn (encode jedi)


main :: IO ()
main = do
    putStrLn $ "Listening on http://localhost:" ++ (show restPort)
    run restPort $ websocketsOr defaultConnectionOptions wsApp app
