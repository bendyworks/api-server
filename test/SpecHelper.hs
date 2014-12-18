{-# LANGUAGE RankNTypes, QuasiQuotes #-}

module SpecHelper where

import Data.Aeson (FromJSON, decode)
import Api.Server (apiApp, initDb)
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Hasql (IsolationLevel (..), Tx, tx, q, unit)
import Hasql.Postgres (Postgres)
import Network.URI (URI, parseURI)
import Network.Wai (Application)
import Network.Wai.Test (SResponse, simpleBody)
import Text.Email.Validate (EmailAddress, emailAddress)
import Test.Hspec.Wai (WaiExpectation, request, with)
import Test.Hspec.Wai.Internal (WaiSession)

import qualified Api.Mappers.User as User

import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Test.Hspec as H

import Api.Types.Config
import Api.Types.Resource
import Api.Types.Fields
import Api.Types.PendingUserResource
import Api.Types.User
import Network.HTTP.Types

data SRequest = SRequest
  { method  :: StdMethod
  , route   :: SB.ByteString
  , headers :: [Header]
  , params  :: SimpleQuery
  }

sendReq :: SRequest -> WaiSession SResponse
sendReq req = request m r h p
  where
    m = renderStdMethod $ method req
    h = headers req
    r = case method req of
          GET -> SB.concat [route req, renderSimpleQuery True $ params req]
          _   -> route req
    p = case method req of
          GET -> ""
          _   -> LB.fromStrict . renderSimpleQuery False $ params req

-- accessing the application and database

config :: Config
config = defaultConfig
    { config_database = defaultConfig { db_name = "api_test" }
    , config_mailer   = defaultConfig
    , config_server   = defaultConfig { server_env = Test }
    }

withApiApp :: H.SpecWith Application -> H.Spec
withApiApp = with (apiApp config)

withRollback :: ((forall a. (forall s. Tx Postgres s a) -> IO a) -> b) -> b
withRollback spec = spec $ \queries ->
    initDb (config_database config) $
      tx (Just (ReadCommitted, True)) $ do
        _ <- unit [q| SAVEPOINT before_spec |]
        vals <- queries
        _ <- unit [q| ROLLBACK TO SAVEPOINT before_spec |]
        return vals

withQuery :: ((forall a. (forall s. Tx Postgres s a) -> IO a) -> b) -> b
withQuery spec = spec $ \queries ->
    initDb (config_database config) $ tx Nothing queries

resetDb :: IO ()
resetDb =
  initDb (config_database config) $
    tx Nothing $ do
      _ <- unit [q| DELETE FROM pending_user_resources |]
      _ <- unit [q| DELETE FROM users                  |]
      _ <- unit [q| DELETE FROM resources              |]
      return ()

-- mock fields

mockUrl :: String -> URI
mockUrl = fromJust . parseURI

mockEmail :: SB.ByteString -> EmailAddress
mockEmail = fromJust . emailAddress

mockResFields :: ResourceFields
mockResFields = ResourceFields
    (ResourceName "test resource name")
    (ResourceEmail $ mockEmail "test@example.com")
    Nothing

mockPendFields :: Tx Postgres s PendingUCFields
mockPendFields = do
  let email = ResourceEmail $ mockEmail "test@example.com"
  login <- fromJust <$> User.insert
  return $ PendingUCFields (login_userId login) email

-- expectations

shouldBe :: (Show a, Eq a) => a -> a -> WaiExpectation
shouldBe a b = liftIO $ a `H.shouldBe` b

shouldSatisfy :: (Show a, Eq a) => a -> (a -> Bool) -> WaiExpectation
shouldSatisfy a b = liftIO $ a `H.shouldSatisfy` b

-- controller actions

fromJsonBody :: FromJSON a => SResponse -> a
fromJsonBody = fromJust . decode . simpleBody

-- HTTP entities

authHeader :: Login -> Header
authHeader (Login _ token) =
  (hAuthorization, SB.concat ["Token ", toBytes token])

authParam :: Login -> SimpleQueryItem
authParam (Login uid _) = ("user_id", toBytes uid)

formHeader :: Header
formHeader = (hContentType, "application/x-www-form-urlencoded")
