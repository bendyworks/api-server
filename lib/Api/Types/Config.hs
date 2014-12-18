{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.Types.Config where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word16, Word32)

class Configurable a where
  defaultConfig :: a

data ApiEnvironment
  = Development
  | Test
  | Production
  deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { db_name             :: Text
  , db_user             :: Text
  , db_password         :: Text
  , db_host             :: ByteString
  , db_port             :: Word16
  , db_keepAliveSeconds :: NominalDiffTime
  , db_maxConnsCount    :: Word32
  } deriving (Eq, Show)

instance Configurable DatabaseConfig where
  defaultConfig = DatabaseConfig
    { db_name             = "api_dev"
    , db_user             = "postgres"
    , db_password         = "\"\""
    , db_host             = "localhost"
    , db_port             = 5432
    , db_keepAliveSeconds = 120
    , db_maxConnsCount    = 20
    }

data MailerConfig = MailerConfig
  { mail_server   :: String
  , mail_user     :: String
  , mail_password :: String
  , mail_port     :: Word16
  } deriving (Eq, Show)

instance Configurable MailerConfig where
  defaultConfig = MailerConfig
    { mail_server   = "localhost"
    , mail_user     = "user"
    , mail_password = "password"
    , mail_port     = 25
    }

data ServerConfig = ServerConfig
  { server_env  :: ApiEnvironment
  , server_port :: Int
  } deriving (Eq, Show)

instance Configurable ServerConfig where
  defaultConfig = ServerConfig
    { server_env  = Development
    , server_port = 3000
    }

data Config = Config
  { config_database :: DatabaseConfig
  , config_mailer   :: MailerConfig
  , config_server   :: ServerConfig
  } deriving (Eq, Show)

instance Configurable Config where
  defaultConfig = Config defaultConfig defaultConfig defaultConfig
