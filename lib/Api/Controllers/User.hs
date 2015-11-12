module Api.Controllers.User
( authenticate
, create
, unverifiedEdit
, verifyEdit
) where

import Api.Types.Fields (UserToken (..))
import Api.Types.Server (ApiActionM, ApiException (..), mailer)
import Control.Monad.Reader (asks, lift)
import Control.Applicative ((<$>), (<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Web.Scotty.Trans (header, json, raise)

import qualified Api.Mailers.Verify              as Verify
import qualified Api.Mappers.Resource            as Resource
import qualified Api.Mappers.PendingUserResource as Pending
import qualified Api.Mappers.User                as User
import qualified Data.Text.Lazy                  as LT
import qualified Data.Text                       as ST

import Api.Helpers.Controller
import Api.Types.Resource
import Api.Types.PendingUserResource
import Api.Types.User

authenticate :: ApiActionM s User
authenticate = do
    foundUser <- loginFromHeader
    case foundUser of
      Just user -> return user
      _ -> raise UnauthorizedUser

-- only used to register a new device

create :: ApiActionM s ()
create = reqQuery User.insert >>= json

-- only used after registration when a user needs to initially connect to a
-- contact

unverifiedEdit :: User -> ApiActionM s ()
unverifiedEdit _ = do
  sendEmail <- lift $ asks mailer
  fields    <- fromParams
  pending   <- reqQuery $ Pending.insert fields
  liftIO . sendEmail $ Verify.mkEmail pending
  json ("ok" :: ST.Text)

verifyEdit :: ApiActionM s ()
verifyEdit = do
    uuid <- reqParam "uuid"
    user <- reqQuery $ runMaybeT $ do
      pending  <- MaybeT $ Pending.findByUuid uuid
      _        <- MaybeT $ Just <$> Pending.delete (pend_id pending)
      resource <-     MaybeT (Resource.findByEmail $ email pending)
                 <|> MaybeT (Resource.insert . fromEmail $ email pending)
      MaybeT . User.update $ User (uid pending) (Just $ res_id resource)
    json user
  where
    uid   = pend_userId . pend_fields
    email = pend_resourceEmail . pend_fields

-- private functions

loginFromHeader :: ApiActionM s (Maybe User)
loginFromHeader = do
    authToken <- header "Authorization"
    case LT.words <$> authToken of
      Just ["Token", token] -> do
        uid <- reqParam "user_id"
        query $ User.findByLogin . Login uid . UserToken $ LT.toStrict token
      _ -> raise MissingAuthToken
