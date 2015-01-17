{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

import           Control.Applicative        (Applicative, (<$>), (<*>))
import           Control.Exception          as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.Error.Class  (MonadError, catchError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks,
                                             runReaderT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Control.Monad.Trans.Maybe  (MaybeT, runMaybeT)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Attoparsec.Number
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char                  (toLower, toUpper)
import           Data.List                  (intercalate)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, maybe)
import           Data.Maybe                 (listToMaybe)
import qualified Data.Text                  as T
import           Network.Wreq
import           System.Environment         (getArgs, getEnv)

type Word = String

newtype SynonymList = SynonymList {synonyms :: [Word]} deriving (Eq, Show)

instance FromJSON SynonymList where
  parseJSON (Object v) = SynonymList <$> (v .: "synonyms")

data WordsConfig = WordsConfig
                    { accessToken :: String }

newtype WordsApp a = WordsApp
                    { runW :: EitherT WordsError (ReaderT WordsConfig IO) a }
                    deriving (Functor, Applicative, Monad,
                              MonadReader WordsConfig, MonadIO,
                              MonadError WordsError)

data WordsError = WordNotFound
                | InvalidConfig
                | InvalidArgs
                deriving (Eq, Show, Read)

class Display d where
  display :: d -> String

instance Display WordsError where
  display WordNotFound = "word not found"
  display InvalidConfig = "invalid configuration"
  display InvalidArgs = "invalid arguments"

data RequestType = Synonyms
                 | Antonyms
                 deriving (Eq, Show, Read)

data WordsRequest = WordsRequest { requestType :: RequestType
                                 , word        :: Word } deriving (Eq, Show)

type WordsEnv = EitherT WordsError IO

runWordsApp :: WordsApp a -> WordsConfig -> IO (Either WordsError a)
runWordsApp w cfg = runReaderT (runEitherT (runW w)) cfg

getConfig :: WordsEnv WordsConfig
getConfig = do
  e <- liftIO $ try (getEnv "WORDS_TOKEN")
  case e of
    Left (SomeException _) -> throwError InvalidConfig
    Right token -> return (WordsConfig token)

guard' :: Bool -> WordsError -> WordsEnv ()
guard' b e = if b then return () else throwError e

capitalize :: String -> String
capitalize [] = []
capitalize (h:t) = toUpper h : map toLower t

parseReqString :: String -> WordsEnv RequestType
parseReqString s = do
  let maybeRes = (listToMaybe . reads) (capitalize s) :: Maybe (RequestType, String)
  (reqType, excess) <- maybe (throwError InvalidArgs) return maybeRes
  guard' (null excess) InvalidArgs
  return reqType

parseRequest :: WordsEnv WordsRequest
parseRequest = do
  args <- liftIO getArgs
  guard' (length args == 2) InvalidArgs
  let [reqString, word] = args
  reqType <- parseReqString reqString
  return $ WordsRequest reqType word

app :: WordsEnv String
app = do
  cfg <- getConfig
  req <- parseRequest
  EitherT $ runWordsApp (handleRequest req) cfg

main :: IO ()
main = (runEitherT safe) >>= displayResults where
  safe = app `catchError` errorHandler

displayResults :: Either WordsError String -> IO ()
displayResults (Left e) = putStrLn $ display e
displayResults (Right r) = putStrLn r

errorHandler :: WordsError -> WordsEnv String
errorHandler = return . display

handleRequest :: WordsRequest -> WordsApp String
handleRequest WordsRequest{..} = case requestType of
  Synonyms -> liftM (intercalate ", ") (getSynonyms word)
  _ -> throwError InvalidArgs

getSynonyms :: Word -> WordsApp [Word]
getSynonyms w = do
  json <- getJSONEndpoint w "synonyms"
  let res = decode json :: Maybe SynonymList
  maybe (throwError WordNotFound) (return . synonyms) res

type URL = String

baseURL :: URL
baseURL = "http://www.wordsapi.com/words"

buildURL :: Word -> String -> URL
buildURL w endpoint = intercalate "/" [baseURL, w, endpoint]

tryGetWith :: Options -> URL -> WordsApp (Response C.ByteString)
tryGetWith opts url = do
  req <- liftIO $ (E.try $ getWith opts url :: IO (Either SomeException (Response C.ByteString)))
  case req of
    Left _ -> throwError WordNotFound
    Right res -> return res

getJSON :: URL -> WordsApp C.ByteString
getJSON url = do
  token <- asks accessToken
  let opts = defaults & param "accessToken" .~ [T.pack token]
  let handler _ = mzero
  liftM (^. responseBody) $ tryGetWith opts url

getJSONEndpoint :: Word -> String -> WordsApp C.ByteString
getJSONEndpoint w e = getJSON (buildURL w e)
