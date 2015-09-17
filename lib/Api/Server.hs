{-# LANGUAGE RankNTypes #-}

module Api.Server
( apiServer
, apiApp
, initDb
) where

import Api.Routes (routes)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (fromJust)
import Data.Text.Encoding (encodeUtf8)
import Hasql (Session, session, sessionSettings, sessionUnlifter)
import Hasql.Postgres (Postgres, Settings(ParamSettings))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Mail.Mime (Mail, renderSendMail)
import Network.Mail.SMTP (sendMailWithLogin')
import Network.Socket.Internal (PortNumber (PortNum))
import Web.Scotty.Trans

import Api.Types.Config
import Api.Types.Server

-- public functions

apiApp :: Config -> IO Application
apiApp config =
    initDb (config_database config) $ do
      db <- sessionUnlifter
      let state = ServerState $ initMailer config
      scottyAppT (`runReaderT` state) (\r -> db $ runReaderT r state) routes

apiServer :: Config -> IO ()
apiServer config = apiApp config >>= run port
  where
    port = server_port $ config_server config

initDb :: DatabaseConfig -> (forall s. Session Postgres s IO r) -> IO r
initDb con = session postgres connect
  where
    connect = fromJust $ sessionSettings
      (db_maxConnsCount con)
      (db_keepAliveSeconds con)
    postgres = ParamSettings
      (db_host con)
      (db_port con)
      (encodeUtf8 (db_user con))
      (encodeUtf8 (db_password con))
      (encodeUtf8 (db_name con))

-- private functions

initMailer :: Config -> Mail -> IO ()
initMailer config =
    case server_env $ config_server config of
      Test        -> const $ return ()
      Development -> renderSendMail
      _           -> sendMailWithLogin' server (PortNum port) user pass
  where
    server     = mail_server mailConfig
    user       = mail_user mailConfig
    pass       = mail_password mailConfig
    port       = mail_port mailConfig
    mailConfig = config_mailer config
