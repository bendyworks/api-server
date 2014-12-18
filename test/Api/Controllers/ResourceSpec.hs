--{-# LANGUAGE QuasiQuotes #-}

module Api.Controllers.ResourceSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.Aeson (toJSON)
import Data.Maybe (fromJust)

import Api.Mappers.Resource as Resource
import Api.Mappers.User as User

import Api.Types.Resource
import Api.Types.Fields
import Api.Types.User
import Network.HTTP.Types
import SpecHelper
import Test.Hspec hiding (shouldBe, shouldSatisfy, pendingWith)
import Test.Hspec.Wai hiding (request)
import Test.Hspec.Wai.JSON (fromValue)

main :: IO ()
main = hspec spec

spec :: Spec
spec = before resetDb $ withApiApp $ do

  describe "GET /resource" $ do

    let request = SRequest
          { method  = GET
          , route   = "/resource"
          , headers = []
          , params  = [] }

    context "not as an authenticated user" $

      it "returns a 401" $
        sendReq request `shouldRespondWith` 401

    context "as an authenticated user" $ do
      context "without an associated Resource" $

        it "returns a 404" $ withQuery $ \query -> do
          login <- liftIO $ query $ fromJust <$> User.insert

          let missing404 = request
                { headers = [authHeader login, formHeader]
                , params  = [authParam login ]
                }

          sendReq missing404 `shouldRespondWith` 404

      context "with an associated Resource" $

        it "returns the given user's Resource info as JSON" $ withQuery $ \query -> do
          (login, resource) <- liftIO $ query $ do
            resource <- fromJust <$> Resource.insert mockResFields
            login    <- fromJust <$> User.insert
            let user = User (login_userId login) (Just $ res_id resource)
            _ <- fromJust <$> User.update user
            return (login, resource)

          let success200 = request
                { headers = [authHeader login]
                , params  = [authParam login] }

          response <- sendReq success200
          return response `shouldRespondWith` 200
          return response `shouldRespondWith` fromValue (toJSON resource)

  describe "POST /resource" $ do

    let request = SRequest
          { method  = POST
          , route   = "/resource"
          , headers = []
          , params  = [] }

    context "not as an authenticated user" $

      it "returns a 401" $
        sendReq request `shouldRespondWith` 401

    context "as an authenticated user" $ do
      context "without an associated Resource" $

        it "returns a 404" $ withQuery $ \query -> do
          login <- liftIO $ query $ fromJust <$> User.insert

          let missing404 = request
                { headers = [authHeader login, formHeader]
                , params  = [authParam login ]
                }

          sendReq missing404 `shouldRespondWith` 404

      context "with an associated Resource" $ do
        context "with invalid params" $

          it "returns a 422" $ withQuery $ \query -> do
            login <- liftIO $ query $ do
              resource <- fromJust <$> Resource.insert mockResFields
              login    <- fromJust <$> User.insert
              let user = User (login_userId login) (Just $ res_id resource)
              _ <- fromJust <$> User.update user
              return login

            let malformed422 = request
                  { headers = [authHeader login, formHeader]
                  , params  = [authParam login
                              ,("resource_name" , toBytes $ res_name mockResFields)
                              ]}

            sendReq malformed422 `shouldRespondWith` 422

        context "with valid params" $

          it "updates and returns the user's Resource info as JSON" $ withQuery $ \query -> do
            login <- liftIO $ query $ do
              resource <- fromJust <$> Resource.insert mockResFields
              login    <- fromJust <$> User.insert
              let user = User (login_userId login) (Just $ res_id resource)
              _ <- fromJust <$> User.update user
              return login

            let newEmailAddr = ResourceEmail $ mockEmail "newResourceEmail@example.com"
                success200 = request
                  { headers = [authHeader login, formHeader]
                  , params  = [authParam login
                              ,("resource_name"    , toBytes $ res_name mockResFields)
                              ,("resource_email"   , toBytes newEmailAddr)
                              ]}

            response <- sendReq success200
            return response `shouldRespondWith` 200

            let updated = fromJsonBody response
            res_email updated `shouldBe` newEmailAddr
