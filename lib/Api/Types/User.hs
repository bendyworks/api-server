{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api.Types.User
( User (..)
, Login (..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Hasql (RowParser, parseRow)
import Hasql.Postgres (Postgres)
import Data.Aeson ((.=), (.:), FromJSON, ToJSON, Value(..), parseJSON, toJSON, object)

import Api.Types.Fields

-- User type

data User = User
  { user_id        :: UserID
  , user_resourceId :: Maybe ResourceID
  } deriving (Eq, Show)

instance ToJSON User where
  toJSON (User uid rid) =
    object [ "user_id"     .= uid
           , "resource_id" .= rid
           ]

instance RowParser Postgres User where
  parseRow row = parseRow row >>= \(a,b) -> do
    let a' = UserID a
        b' = ResourceID <$> b
    return $ User a' b'

-- Login type

data Login = Login
  { login_userId    :: UserID
  , login_userToken :: UserToken
  } deriving (Eq, Show)

instance FromJSON Login where
  parseJSON (Object v) =
    Login <$> v .: "user_id"
                 <*> v .: "api_token"
  parseJSON _ = mzero

instance ToJSON Login where
  toJSON (Login uid token) =
    object [ "user_id"   .= uid
           , "api_token" .= token
           ]

instance RowParser Postgres Login where
  parseRow row = parseRow row >>= \(a,b) -> do
    let a' = UserID a
        b' = UserToken b
    return $ Login a' b'
