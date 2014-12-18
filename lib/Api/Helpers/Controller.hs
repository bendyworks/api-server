{-# LANGUAGE RankNTypes #-}

module Api.Helpers.Controller where

import Control.Applicative ((<$>))
import Control.Monad.Reader (lift)
import Hasql (IsolationLevel (..), Tx, tx)
import Hasql.Postgres (Postgres)

import qualified Data.Text.Lazy as T

import Api.Types.Server
import Network.HTTP.Types
import Web.Scotty.Trans

-- constants

query :: (forall s1. Tx Postgres s1 a) -> ApiActionM s a
query q = lift . lift $ tx (Just (ReadCommitted, True)) q

reqQuery :: (forall s1. Tx Postgres s1 (Maybe a)) -> ApiActionM s a
reqQuery q = do
  results <- query q
  case results of
    Just r -> return r
    _      -> raise NoQueryResults

-- param fetching

optParam :: (Parsable a) => T.Text -> ApiActionM s (Maybe a)
optParam key = do
  val <- lookup key <$> params
  case val of
    Just unparsed -> case parseParam unparsed of
      Left _  -> raise $ MalformedParam key
      Right x -> return $ Just x
    _ -> return Nothing

reqParam :: (Parsable a) => T.Text -> ApiActionM s a
reqParam key = do
  val <- optParam key
  case val of
    Just x -> return x
    _      -> raise $ MissingParam key

class FromParams a where
  fromParams :: ApiActionM s a

-- route helpers

status422 :: Status
status422 = mkStatus 422 "unprocessable entity"
