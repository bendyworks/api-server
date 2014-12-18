module Api.Controllers.Resource where

import Prelude hiding (show)

import Api.Types.Server (ApiActionM, ApiException (..))
import Control.Applicative ((<$>))
import Web.Scotty.Trans (json, raise)

import qualified Api.Mappers.Resource as Resource

import Api.Helpers.Controller
import Api.Types.Resource
import Api.Types.Fields
import Api.Types.User

show :: User -> ApiActionM s ()
show user = do
  cid     <- getResourceIdFor user
  resource <- reqQuery $ Resource.find cid
  json resource

-- TODO: if the email address is updated, we shouldn't update it unless
-- the User clicks a link in their email ala `User.unverifiedEdit`
edit :: User -> ApiActionM s ()
edit user = do
  rid      <- getResourceIdFor user
  resource <- Resource rid <$> fromParams
  saved    <- reqQuery $ Resource.update resource
  json saved

-- private

getResourceIdFor :: User -> ApiActionM s ResourceID
getResourceIdFor user =
  case user_resourceId user of
    (Just rid) -> return rid
    _          -> raise NoResourceForUser
