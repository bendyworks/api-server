{-# LANGUAGE QuasiQuotes #-}

module Api.Controllers.UserSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Hasql (q, single)

import Api.Mappers.Resource as Resource
import Api.Mappers.User as User
import Api.Mappers.PendingUserResource as Pending

import Api.Types.Resource
import Api.Types.Fields
import Api.Types.PendingUserResource
import Api.Types.User
import Network.HTTP.Types
import SpecHelper
import Test.Hspec hiding (shouldBe, shouldSatisfy, pendingWith)
import Test.Hspec.Wai hiding (request)


main :: IO ()
main = hspec spec

spec :: Spec
spec = before resetDb $ withApiApp $ do

  describe "POST /users" $ do

    let request = SRequest
          { method  = POST
          , route   = "/users"
          , headers = []
          , params  = [] }

    it "returns a user's Login via JSON" $ withQuery $ \query -> do
      responseWithLogin <- sendReq request
      let login = fromJsonBody responseWithLogin
          (UserID uid) = login_userId login
      users <- liftIO $ query $ single $
        [q| SELECT COUNT(id) FROM users WHERE id = ? |] uid

      return responseWithLogin `shouldRespondWith` 200
      fromJust users `shouldBe` Identity (1 :: Int)

  describe "GET /verify/:uuid" $ do

    let request = SRequest
          { method  = GET
          , route   = "/verify/xxx"
          , headers = []
          , params  = [] }

    context "UUID references an existing PendingUserResource" $ do
      context "Email of PendingUserResource doesn't match an existing resource" $

        it "connects the User with a new Resource" $ withQuery $ \query -> do
          let email = ResourceEmail $ mockEmail "test@example.com"

          (login, pend) <- liftIO $ query $ do
            login <- fromJust <$> User.insert
            pend  <- fromJust <$> Pending.insert (PendingUCFields (login_userId login) email)
            return (login, pend)

          let success200 = request { route = "/verify/" <> toBytes (pend_uuid pend) }
          sendReq success200 `shouldRespondWith` 200

          (newRes, delPend, user) <- liftIO $ query $ do
            delPend <- Pending.findByUuid (pend_uuid pend)
            newRes  <- fromJust <$> Resource.findByEmail email
            user    <- fromJust <$> User.findByLogin login
            return (newRes, delPend, user)

          delPend `shouldBe` Nothing
          user_resourceId user `shouldBe` Just (res_id newRes)

      context "Email of PendingUserResource matches an existing resource" $

        it "connects the User to the existing Resource" $ withQuery $ \query -> do
          (login, pend, resource) <- liftIO $ query $ do
            login     <- fromJust <$> User.insert
            resource   <- fromJust <$> Resource.insert mockResFields
            let fields = PendingUCFields (login_userId login) (res_email mockResFields)
            pend      <- fromJust <$> Pending.insert fields
            return (login, pend, resource)

          let success200 = request { route = "/verify/" <> toBytes (pend_uuid pend) }
          sendReq success200 `shouldRespondWith` 200

          user <- liftIO $ query $ fromJust <$> User.findByLogin login
          user_resourceId user `shouldBe` Just (res_id resource)

    context "UUID doesn't reference an existing PendingUserResource" $

      it "returns a 404" $
        sendReq request `shouldRespondWith` 404

  describe "POST /users/:user_id" $ do

    let request = SRequest
          { method  = POST
          , route   = "/users/1"
          , headers = []
          , params  = [] }

    context "without an authenticated user" $

      it "returns a 401" $
        sendReq request `shouldRespondWith` 401

    context "with an authenticated user" $ do
      context "without valid params" $ do

        it "returns a 422 with invalid params" $ withQuery $ \query -> do
          login <- liftIO $ query $ do
            login    <- fromJust <$> User.insert
            resource <- fromJust <$> Resource.insert mockResFields
            _ <- User.update $ User (login_userId login) (Just $ res_id resource)
            return login

          let malformed422 = request
                { route   = "/users/" <> toBytes (login_userId login)
                , headers = [authHeader login, formHeader]
                , params  = [ authParam login
                            , ("resource", "invalid_email")
                            ] }

          sendReq malformed422 `shouldRespondWith` 422

        it "returns a 422 with missing params" $ withQuery $ \query -> do
          login <- liftIO $ query $ do
            login   <- fromJust <$> User.insert
            resource <- fromJust <$> Resource.insert mockResFields
            _ <- User.update $ User (login_userId login) (Just $ res_id resource)
            return login

          let malformed422 = request
                { route   = "/users/" <> toBytes (login_userId login)
                , headers = [authHeader login, formHeader]
                , params  = [authParam login] }

          sendReq malformed422 `shouldRespondWith` 422

      context "with valid params" $ do

        it "creates a new PendingUserResource" $ withQuery $ \query -> do
          (login, resource) <- liftIO $ query $ do
            login   <- fromJust <$> User.insert
            resource <- fromJust <$> Resource.insert mockResFields
            _ <- User.update $ User (login_userId login) (Just $ res_id resource)
            return (login, resource)

          let success200 = request
                { route   = "/users/" <> toBytes (login_userId login)
                , headers = [authHeader login, formHeader]
                , params  = [ authParam login
                            , ("resource_email", toBytes . res_email $ res_fields resource)
                            ] }

          sendReq success200 `shouldRespondWith` 200
          count <- liftIO $ query $ single [q| SELECT COUNT(id) from pending_user_resources |]
          fromJust count `shouldBe` Identity (1 :: Int)

        it "sends a verification email with a PendingUUID" $
          pendingWith "need way to test mailer contents"
