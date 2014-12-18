module Api.Mappers.ResourceSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust, isJust)

import qualified Api.Mappers.Resource as Resource

import Api.Types.Fields
import Api.Types.Resource
import SpecHelper hiding (shouldBe, shouldSatisfy)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = beforeAll resetDb $ do

  describe "find" $ do

    it "finds a Resource by its ID" $ withRollback $ \q -> do
      found <- q $ do
        resource <- fromJust <$> Resource.insert mockResFields
        Resource.find $ res_id resource

      res_fields (fromJust found) `shouldBe` mockResFields

    it "returns Nothing if no Resource with the UUID exists" $ withRollback $ \q -> do
      results <- q $ Resource.find (ResourceID 9999)

      results `shouldBe` Nothing

  describe "findByEmail" $ do

    it "finds a Resource by its ResourceEmail" $ withRollback $ \q -> do
      found <- q $ do
        _ <- Resource.insert mockResFields
        Resource.findByEmail (res_email mockResFields)

      found `shouldSatisfy` isJust

    it "returns Nothing if no Resource with the ResourceEmail exists" $ withRollback $ \q -> do
      let email = ResourceEmail $ mockEmail "nonexistant@email.com"
      results <- q $ Resource.findByEmail email

      results `shouldBe` Nothing

  describe "insert" $ do

    it "creates a new Resource" $ withRollback $ \q -> do
      resource <- q $ fromJust <$> Resource.insert mockResFields

      res_fields resource `shouldBe` mockResFields

    it "requires a unique email address" $ withRollback $ \q -> do
      let res1     = mockResFields { res_name = ResourceName "res1" }
          res2     = mockResFields { res_name = ResourceName "res2" }
          badQuery = q $ Resource.insert res1 >> Resource.insert res2

      badQuery `shouldThrow` anyException

  describe "update" $ do

    it "updates a Resource with the same ID" $ withRollback $ \q -> do
      let oldres = mockResFields { res_name = ResourceName "old" }
          newres = mockResFields { res_name = ResourceName "new" }

      resource <- q $ do
        old <- fromJust <$> Resource.insert oldres
        _   <- Resource.update $ old { res_fields = newres }
        fromJust <$> Resource.find (res_id old)

      res_fields resource `shouldBe` newres

    it "returns Nothing if no Resource has the same ResourceID" $ withRollback $ \q -> do
      let notInDb = Resource (ResourceID 9999) mockResFields
      found <- q $ Resource.update notInDb

      found `shouldBe` Nothing
