{-# LANGUAGE QuasiQuotes #-}

module Api.Mappers.UserSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromJust, isJust)
import Hasql (q, single)

import qualified Api.Mappers.Resource as Resource
import qualified Api.Mappers.User as User

import Api.Types.Fields
import Api.Types.Resource
import Api.Types.User
import SpecHelper hiding (shouldBe, shouldSatisfy)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll resetDb $ do

  describe "findByLogin" $ do

    it "finds a User by their UserID and UserToken" $ withRollback $ \query -> do
      found <- query $ do
        login    <- fromJust <$> User.insert
        resource <- fromJust <$> Resource.insert mockResFields
        _        <- User.update $ User (login_userId login) (Just $ res_id resource)
        User.findByLogin login

      found `shouldSatisfy` isJust

    it "returns Nothing if the UserTokens don't match" $ withRollback $ \query -> do
      found <- query $ do
        login <- fromJust <$> User.insert
        let badLogin = Login (login_userId login) (UserToken "invalid_token")
        User.findByLogin badLogin

      found `shouldBe` Nothing

    it "returns Nothing if the UserIDs don't match" $ withRollback $ \query -> do
      found <- query $ do
        login <- fromJust <$> User.insert
        let badLogin = Login (UserID 9999) (login_userToken login)
        User.findByLogin badLogin

      found `shouldBe` Nothing

  describe "insert" $ do

    it "creates a new User" $ withRollback $ \query -> do
      count <- query $ do
        login <- fromJust <$> User.insert
        let (UserID uid) = login_userId login
        single $ [q| SELECT COUNT(id) FROM users WHERE id = ? |] uid

      fromJust count `shouldBe` Identity (1 :: Int)

    it "generates unique tokens for each User" $ withRollback $ \query -> do
      hasSameTokens <- query $ do
        login1 <- fromJust <$> User.insert
        login2 <- fromJust <$> User.insert
        return $ login_userToken login1 == login_userToken login2

      hasSameTokens `shouldBe` False

  describe "update" $ do

    it "updates a User with the same ID" $ withRollback $ \query -> do
      (user, resource) <- query $ do
        resource <- fromJust <$> Resource.insert mockResFields
        login    <- fromJust <$> User.insert
        user     <- fromJust <$> User.update (User (login_userId login) (Just $ res_id resource))
        return (user, resource)

      user_resourceId user `shouldBe` Just (res_id resource)

    it "returns Nothing if no User has the same UserID" $ withRollback $ \query -> do
      let notInDb = User (UserID 9999) (Just $ ResourceID 1)
      found <- query $ User.update notInDb

      found `shouldBe` Nothing
