{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           Control.Applicative        (Applicative, (<$>), (<*>))
import           Control.Exception          as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks,
                                             runReaderT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Attoparsec.Number
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, maybe)
import qualified Data.Text                  as T
import           Network.Wreq
import           System.Environment         (getArgs, getEnv)

type Word = String

newtype Synonyms = Synonyms {synonyms :: [Word]} deriving (Eq, Show)

data WordleConfig = WordleConfig
                    { accessToken :: String }

newtype Wordle a = Wordle
                    { runW :: MaybeT (ReaderT WordleConfig IO) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader WordleConfig, MonadIO, MonadPlus)

runWordle :: Wordle a -> WordleConfig -> IO (Maybe a)
runWordle w cfg = runReaderT (runMaybeT (runW w)) cfg

instance FromJSON Synonyms where
  parseJSON (Object v) = Synonyms <$> (v .: "synonyms")

getConfig :: IO WordleConfig
getConfig = getEnv "WORDLE_TOKEN" >>= return . WordleConfig

main :: IO ()
main = do
  cfg <- getConfig
  args <- getArgs
  if valid args
    then handleWordleReq args cfg
    else putStrLn "Invalid request"

valid :: [String] -> Bool
valid = (==2) . length

handleWordleReq :: [String] -> WordleConfig -> IO ()
handleWordleReq (t:w:_) cfg = do
  case t of
    "synonyms" -> do
      res <- runWordle (getSynonyms w) cfg
      putStrLn $ maybe "No synonyms found" unwords res
    _ -> putStrLn $ "unrecognized request: " ++ t

getSynonyms :: Word -> Wordle [Word]
getSynonyms w = do
  json <- getJSONEndpoint w "synonyms"
  let res = decode json :: Maybe Synonyms
  maybe mzero (return . synonyms) res

type URL = String

baseURL :: URL
baseURL = "http://www.wordsapi.com/words"

buildURL :: Word -> String -> URL
buildURL w endpoint = intercalate "/" [baseURL, w, endpoint]

tryGetWith :: Options -> URL -> IO (Maybe (Response C.ByteString))
tryGetWith opts url = do
  req <- E.try $ getWith opts url :: IO (Either SomeException (Response C.ByteString))
  case req of
    Left _ -> return Nothing
    Right res -> return (Just res)

getJSON :: URL -> Wordle C.ByteString
getJSON url = do
  token <- asks accessToken
  let opts = defaults & param "accessToken" .~ [T.pack token]
  let handler _ = mzero
  r <- liftIO $ tryGetWith opts url
  maybe mzero (return . (^. responseBody)) r

getJSONEndpoint :: Word -> String -> Wordle C.ByteString
