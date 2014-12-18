{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Types.Fields where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, ToJSON, Value(..), parseJSON, toJSON)
import Hasql.Backend (Mapping (..))
import Hasql.Postgres (Postgres)
import Text.Email.Validate (EmailAddress, emailAddress, toByteString)
import Web.Scotty.Trans (Parsable, parseParam)

import qualified Data.ByteString            as SB
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as ST
import qualified Data.Text.Encoding         as SE
import qualified Data.Text.Lazy.Encoding    as LE

-- newtype-wrapped database fields

newtype UserID          = UserID Int                  deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)
newtype UserToken       = UserToken ST.Text           deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)

newtype PendingID       = PendingID Int               deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)
newtype PendingUUID     = PendingUUID ST.Text         deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)

newtype ResourceID       = ResourceID Int             deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)
newtype ResourceName     = ResourceName ST.Text       deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)
newtype ResourceEmail    = ResourceEmail EmailAddress deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)
newtype ResourceOptional = ResourceOptional ST.Text   deriving (Byteable, Eq, Show, FromJSON, ToJSON, Parsable)

-- Email address instances

instance Mapping Postgres EmailAddress where
  renderValue = renderValue . SE.decodeUtf8 . toBytes
  parseResult sql =
    parseResult sql >>= \text ->
      case emailAddress $ SE.encodeUtf8 text of
        Just email -> Right email
        _          -> Left "could not parse email from SQL"

instance FromJSON EmailAddress where
  parseJSON (String text) = maybe mzero return . emailAddress $ SE.encodeUtf8 text
  parseJSON _             = mzero

instance ToJSON EmailAddress where
  toJSON = String . SE.decodeUtf8 . toByteString

instance Parsable EmailAddress where
  parseParam emailText =
    case emailAddress . LB.toStrict $ LE.encodeUtf8 emailText of
      Just email -> Right email
      _          -> Left "could not parse email from parameter"

instance Byteable EmailAddress where
  toLazyBytes = LB.fromStrict . toByteString

-- conversions to lazy bytestrings (for emails)

class Byteable a where
  toBytes :: a -> SB.ByteString
  toBytes = LB.toStrict . toLazyBytes
  toLazyBytes :: a -> LB.ByteString
  toLazyBytes = LB.fromStrict . toBytes

instance Byteable ST.Text where
  toBytes = SE.encodeUtf8

instance Byteable Int where
  toLazyBytes = LC.pack . show
