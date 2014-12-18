{-# LANGUAGE QuasiQuotes #-}

module Api.Types.ResourceSpec (main, spec) where

import Data.Aeson (Result (..), Value, fromJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Api.Types.Resource
import Api.Types.Fields
import SpecHelper hiding (shouldBe, shouldSatisfy)
import Test.Hspec

main :: IO ()
main = hspec spec

fields :: ResourceFields
fields = ResourceFields
  (ResourceName "test name")
  (ResourceEmail $ mockEmail "test@example.com")
  (Just $ ResourceOptional "optional")

fieldsJson :: Value
fieldsJson = [aesonQQ|
  { resource_name:     "test name"
  , resource_email:    "test@example.com"
  , resource_optional:  "optional"
  } |]

spec :: Spec
spec = do

  describe "ResourceFields" $ do

    it "can be deserialized from JSON" $
      fromJSON fieldsJson `shouldBe` Success fields

    it "can be stubbed from an email address" $ do
      let email   = mockEmail "test@example.com"
          created = ResourceFields (ResourceName "test")
                                   (ResourceEmail email)
                                   Nothing
      fromEmail (ResourceEmail email) `shouldBe` created

  describe "Resource" $

    it "can be serialized into JSON" $ do
      let contact = Resource (ResourceID 1) fields
          json    = [aesonQQ|
                      { resource_id:       1
                      , resource_name:     "test name"
                      , resource_email:    "test@example.com"
                      , resource_optional:  "optional"
                      } |]

      toJSON contact `shouldBe` json
