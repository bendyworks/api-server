module Api.Routes (routes) where

import Api.Helpers.Controller (status422)

import qualified Api.Controllers.Resource as Resource
import qualified Api.Controllers.User    as User
import qualified Data.Text.Lazy             as LT

import Api.Types.Server
import Network.HTTP.Types
import Web.Scotty.Trans

routes :: ApiServerM s ()
routes = defaultHandler fallback >> do

    get  "/verify/:uuid"     User.verifyEdit

    post "/users"            User.create
    post "/users/:user_id" $ User.authenticate >>= User.unverifiedEdit

    get  "/resource"       $ User.authenticate >>= Resource.show
    post "/resource"       $ User.authenticate >>= Resource.edit

fallback :: ApiException -> ApiActionM s ()
fallback err = case err of
  MissingAuthToken   -> status status401 >> text "authorization header required"
  UnauthorizedUser   -> status status401 >> text "unauthorized"
  NoQueryResults     -> status status404 >> text "file not found"
  NoResourceForUser  -> status status404 >> text "user has no associated resource"
  (MissingParam p)   -> status status422 >> text (LT.concat ["missing parameter: ", p])
  (MalformedParam p) -> status status422 >> text (LT.concat ["malformed parameter: ", p])
  ServerError t      -> status status500 >> text t
