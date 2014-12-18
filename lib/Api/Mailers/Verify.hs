module Api.Mailers.Verify where

import Network.Mail.Mime hiding (simpleMail)
import Network.Mail.SMTP (simpleMail)
import Text.Email.Validate (toByteString)

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text                  as ST
import qualified Data.Text.Encoding         as SE

import Api.Types.PendingUserResource
import Api.Types.Fields

mkEmail :: PendingUserResource -> Mail
mkEmail pending = simpleMail from to cc bcc subject parts
  where
    from    = Address (Just "Api") "no-reply@example.com"
    to      = [Address Nothing $ addr pending]
    cc      = []
    bcc     = []
    subject = "Verify your email address"
    parts = [Part { partType     = "text/plain"
                  , partEncoding = QuotedPrintableText
                  , partFilename = Nothing
                  , partHeaders  = []
                  , partContent  = body pending
                  }]

-- private functions

body :: PendingUserResource -> LC.ByteString
body pending = LC.concat
  [ "Verify your email by clicking this link:  https://example.com/verify/"
  , uuid pending
  ]

addr :: PendingUserResource -> ST.Text
addr pending = SE.decodeUtf8 $ toByteString e
  where
    (ResourceEmail e) = pend_resourceEmail $ pend_fields pending

uuid :: PendingUserResource -> LC.ByteString
uuid pending = LC.fromStrict $ SE.encodeUtf8 u
  where
    (PendingUUID u) = pend_uuid pending
