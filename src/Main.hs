module Main (main) where

import Api.Server (apiServer)
import Api.Types.Config

import Network.URI (parseURI, URI(..), URIAuth(..))
import System.Environment (getEnv)
import Data.Maybe (fromJust)
import qualified Data.Text as ST
import qualified Data.ByteString.Char8 as SB

main :: IO ()
main = do
  dbConnString <- getEnv "DATABASE_URL"

  mailAddr     <- getEnv "SMTP_SERVER"
  mailPort     <- getEnv "SMTP_PORT"
  mailUser     <- getEnv "SMTP_LOGIN"
  mailPass     <- getEnv "SMTP_PASSWORD"

  env          <- getEnv "HASK_ENV"
  port         <- getEnv "PORT"

  apiServer defaultConfig
    { config_database = dbConfig dbConnString
    , config_mailer   = mailConfig mailAddr mailPort mailUser mailPass
    , config_server   = serverConfig env port
    }

dbConfig :: String -> DatabaseConfig
dbConfig conn = do
  let uri                        = fromJust $ parseURI conn
      URIAuth userInfo host port = fromJust $ uriAuthority uri
      (user, password')          = span (/= ':') userInfo
      password                   = init $ tail password'
      name                       = tail $ uriPath uri
  defaultConfig
    { db_name     = ST.pack name
    , db_user     = ST.pack user
    , db_password = ST.pack password
    , db_host     = SB.pack host
    , db_port     = read $ tail port
    }

mailConfig :: String -> String -> String -> String -> MailerConfig
mailConfig server port user pass =
  defaultConfig
    { mail_server   = server
    , mail_user     = user
    , mail_password = pass
    , mail_port     = read port
    }

serverConfig :: String -> String -> ServerConfig
serverConfig env port = case env of
  "DEVELOPMENT" -> defaultConfig { server_env = Development }
  "TEST"        -> defaultConfig { server_env = Test }
  "PRODUCTION"  -> defaultConfig { server_env = Production, server_port = read port }
  _             -> defaultConfig { server_env = Development }
