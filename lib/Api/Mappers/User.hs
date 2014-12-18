{-# LANGUAGE QuasiQuotes #-}

module Api.Mappers.User
( findByLogin
, insert
, update
) where

import Hasql (Tx, q, single)
import Hasql.Postgres (Postgres)

import Api.Types.Fields
import Api.Types.User

findByLogin :: Login -> Tx Postgres s (Maybe User)
findByLogin (Login (UserID a) (UserToken b)) = single $ [q|
  SELECT id, resource_id
  FROM users
  WHERE id = ? AND crypt(?, token_hash) = token_hash
  |] a b

insert :: Tx Postgres s (Maybe Login)
insert = single [q|
  WITH token_table AS (SELECT uuid_generate_v4() :: VARCHAR AS token)
  INSERT INTO users (token_hash)
  SELECT crypt(token, gen_salt('bf', 8))
  FROM token_table
  RETURNING id, (SELECT token FROM token_table)
  |]

update :: User -> Tx Postgres s (Maybe User)
update user = do
  let (a,b) = extractPrims user
  single $ [q|
    UPDATE users
    SET resource_id = ?
    WHERE id = ?
    RETURNING id, resource_id
  |] b a

-- private

extractPrims :: User -> (Int, Maybe Int)
extractPrims (User (UserID uid) resourceId) = (uid, c)
  where
    c =
      case resourceId of
        Just (ResourceID rid) -> Just rid
        _                     -> Nothing
