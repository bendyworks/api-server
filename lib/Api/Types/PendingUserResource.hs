{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Types.PendingUserResource
( PendingUCFields (..)
, PendingUserResource (..)
) where

import Api.Helpers.Controller (FromParams (..), reqParam)
import Control.Applicative ((<$>), (<*>))
import Hasql (RowParser, parseRow)
import Hasql.Postgres (Postgres)

import Api.Types.Fields

-- PendingUCFields type

data PendingUCFields = PendingUCFields
  { pend_userId        :: UserID
  , pend_resourceEmail :: ResourceEmail
  } deriving (Eq, Show)

instance FromParams PendingUCFields where
  fromParams = PendingUCFields <$> reqParam "user_id"
                               <*> reqParam "resource_email"

-- PendingUserResource type

data PendingUserResource = PendingUserResource
  { pend_id     :: PendingID
  , pend_uuid   :: PendingUUID
  , pend_fields :: PendingUCFields
  } deriving (Eq, Show)

instance RowParser Postgres PendingUserResource where
  parseRow row = parseRow row >>= \(a,b,c,d) -> do
    let a' = PendingID    a
        b' = PendingUUID  b
        c' = UserID       c
        d' = ResourceEmail d
    return . PendingUserResource a' b' $ PendingUCFields c' d'
