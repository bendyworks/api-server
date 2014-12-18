{-# LANGUAGE RankNTypes #-}

module Api.Types.Server where

import Control.Monad.Reader (ReaderT)
import Data.Text.Lazy (Text, pack)
import Hasql (Session)
import Hasql.Postgres (Postgres)
import Network.Mail.Mime (Mail)
import Web.Scotty.Trans (ActionT, ScottyError(..), ScottyT)

data ApiException
  = MalformedParam Text
  | MissingParam Text
  | NoQueryResults
  | MissingAuthToken
  | UnauthorizedUser
  | NoResourceForUser
  | ServerError Text
  deriving (Eq, Read, Show)

instance ScottyError ApiException where
  stringError = ServerError . pack
  showError   = pack . show

data ServerState = ServerState
  { mailer   :: Mail -> IO ()
  }

type ApiServerM s = ScottyT ApiException (ReaderT ServerState (Session Postgres s IO))
type ApiActionM s = ActionT ApiException (ReaderT ServerState (Session Postgres s IO))
