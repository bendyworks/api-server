{-# LANGUAGE QuasiQuotes #-}

module Api.Types.UserSpec (main, spec) where

import Data.Aeson (Result (..), fromJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Api.Types.Fields
import Api.Types.User
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "Login" $ do

    let login = Login (UserID 1) (UserToken "token")
        json  = [aesonQQ| { user_id: 1, api_token: "token" } |]

    it "can be serialized to JSON" $
      toJSON login `shouldBe` json

    it "can be deserialized from JSON" $
      fromJSON json `shouldBe` Success login

  describe "User" $ do

    let user = User (UserID 1) (Just $ ResourceID 2)
        json = [aesonQQ| { user_id: 1, resource_id: 2 } |]

    it "can be serialized to JSON" $
      toJSON user `shouldBe` json
