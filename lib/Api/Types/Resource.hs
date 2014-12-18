{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Types.Resource
( Resource (..)
, ResourceFields (..)
, fromEmail
) where

import Api.Helpers.Controller (FromParams (..), optParam, reqParam)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.:?), (.=), FromJSON, ToJSON, Value(..), parseJSON, toJSON, object)
import Data.Text.Encoding (decodeUtf8)
import Hasql (RowParser, parseRow)
import Hasql.Postgres (Postgres)
import Text.Email.Validate (localPart)

import Api.Types.Fields

-- ResourceFields type

data ResourceFields = ResourceFields
  { res_name       :: ResourceName
  , res_email      :: ResourceEmail
  , res_optional   :: Maybe ResourceOptional
  } deriving (Eq, Show)

instance FromJSON ResourceFields where
  parseJSON (Object v) =
    ResourceFields <$> v .:  "resource_name"
                   <*> v .:  "resource_email"
                   <*> v .:? "resource_optional"
  parseJSON _ = mzero

instance FromParams ResourceFields where
  fromParams = ResourceFields <$> reqParam "resource_name"
                              <*> reqParam "resource_email"
                              <*> optParam "resource_optional"

fromEmail :: ResourceEmail -> ResourceFields
fromEmail email@(ResourceEmail e) =
    ResourceFields name email Nothing
  where
    name = ResourceName . decodeUtf8 $ localPart e

-- Resource type

data Resource = Resource
  { res_id     :: ResourceID
  , res_fields :: ResourceFields
  } deriving (Eq, Show)

instance ToJSON Resource where
  toJSON (Resource a (ResourceFields b c d)) =
    object [ "resource_id"       .= a
           , "resource_name"     .= b
           , "resource_email"    .= c
           , "resource_optional" .= d
           ]

instance RowParser Postgres Resource where
  parseRow row = parseRow row >>= \(a,b,c,d) -> do
    let a' =      ResourceID       a
        b' =      ResourceName     b
        c' =      ResourceEmail    c
        d' = fmap ResourceOptional d
    return . Resource a' $ ResourceFields b' c' d'
