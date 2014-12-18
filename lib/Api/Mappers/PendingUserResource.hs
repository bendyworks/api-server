{-# LANGUAGE QuasiQuotes #-}

module Api.Mappers.PendingUserResource where

import Hasql (Tx, q, single, unit)
import Hasql.Postgres (Postgres)

import Api.Types.Fields
import Api.Types.PendingUserResource

findByUuid :: PendingUUID -> Tx Postgres s (Maybe PendingUserResource)
findByUuid (PendingUUID a) = single $ [q|
  SELECT id, uuid, user_id, resource_email
  FROM pending_user_resources
  WHERE uuid = ?
  |] a

insert :: PendingUCFields -> Tx Postgres s (Maybe PendingUserResource)
insert (PendingUCFields (UserID a) (ResourceEmail b)) = single $ [q|
  INSERT INTO pending_user_resources (uuid, user_id, resource_email)
  VALUES (uuid_generate_v4(),?,?)
  RETURNING id, uuid, user_id, resource_email
  |] a b

delete :: PendingID -> Tx Postgres s ()
delete (PendingID a) = unit $ [q|
  DELETE FROM pending_user_resources
  WHERE id = ?
  |] a
