module Api.Mappers.PendingUserResourceSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust, isJust)

import qualified Api.Mappers.PendingUserResource as Pending
import qualified Api.Mappers.User as User

import Api.Types.Fields
import Api.Types.PendingUserResource
import Api.Types.User
import SpecHelper hiding (shouldBe, shouldSatisfy)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  let email1 = ResourceEmail $ mockEmail "test1@email.com"
      email2 = ResourceEmail $ mockEmail "test2@email.com"

  describe "findByUuid" $ do

    it "finds a PendingUserResource by its UUID" $ withRollback $ \q -> do
      results <- q $ do
        fields <- mockPendFields
        pend   <- fromJust <$> Pending.insert fields
        Pending.findByUuid (pend_uuid pend)

      results `shouldSatisfy` isJust

    it "returns Nothing if no PendingUserResource with the UUID exists" $ withRollback $ \q -> do
      results <- q $ Pending.findByUuid $ PendingUUID "invalid_uuid"

      results `shouldBe` Nothing

  describe "insert" $ do

    it "creates a new PendingUserResource" $ withRollback $ \q -> do
      (pend, fields) <- q $ do
        fields <- mockPendFields
        pend   <- fromJust <$> Pending.insert fields
        return (pend, fields)

      pend_fields pend `shouldBe` fields

    it "generates unique UUIDs for each PendingUserResource" $ withRollback $ \q -> do
      hasSameUuids <- q $ do
        login <- fromJust <$> User.insert
        pend1 <- fromJust <$> Pending.insert (PendingUCFields (login_userId login) email1)
        pend2 <- fromJust <$> Pending.insert (PendingUCFields (login_userId login) email2)
        return $ pend_uuid pend1 == pend_uuid pend2

      hasSameUuids `shouldBe` False

    it "rejects duplicate UserID-ResourceEmail pairs" $ withRollback $ \q -> do
      let badQuery = q $ do
            fields <- mockPendFields
            _      <- Pending.insert fields
            Pending.insert fields

      badQuery `shouldThrow` anyException

    it "must reference an existing User" $ withRollback $ \q -> do
      let fields   = PendingUCFields (UserID 9999) email1
          badQuery = q $ Pending.insert fields

      badQuery `shouldThrow` anyException

  describe "delete" $ do

    it "removes the PendingUserResource with the PendingID" $ withRollback $ \q -> do
      results <- q $ do
        fields <- mockPendFields
        pend   <- fromJust <$> Pending.insert fields
        _      <- Pending.delete (pend_id pend)
        Pending.findByUuid (pend_uuid pend)

      results `shouldBe` Nothing

    it "is silent if a PendingUserResource with the PendingID doesn't exist" $ withRollback $ \q -> do
      unit <- q $ Pending.delete $ PendingID 9999

      unit `shouldBe` ()
