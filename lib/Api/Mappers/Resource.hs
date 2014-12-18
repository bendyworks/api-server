{-# LANGUAGE QuasiQuotes #-}

module Api.Mappers.Resource
( find
, findByEmail
, insert
, update
) where

import Hasql (Tx, q, single)
import Hasql.Postgres (Postgres)

import qualified Data.Text as ST
import qualified Data.Text.Encoding as SE

import Api.Types.Resource
import Api.Types.Fields

find :: ResourceID -> Tx Postgres s (Maybe Resource)
find (ResourceID a) = single $ [q|
  SELECT id, name, email, optional
  FROM resources
  WHERE id = ?
  |] a

findByEmail :: ResourceEmail -> Tx Postgres s (Maybe Resource)
findByEmail (ResourceEmail e) = single $ [q|
  SELECT id, name, email, optional
  FROM resources
  WHERE email = ? :: CITEXT
  |] $ SE.decodeUtf8 (toBytes e)

insert :: ResourceFields -> Tx Postgres s (Maybe Resource)
insert cf = do
  let (a, b, c) = extractPrims cf
  single $ [q|
    INSERT INTO resources (name, email, optional)
    VALUES (?,?,?)
    RETURNING id, name, email, optional
  |] a b c

update :: Resource -> Tx Postgres s (Maybe Resource)
update (Resource (ResourceID rid) cf) = do
  let (a, b, c) = extractPrims cf
  single $ [q|
    UPDATE resources
    SET (name, email, optional) = (?,?,?)
    WHERE id = ?
    RETURNING id, name, email, optional
  |] a b c rid

-- private

extractPrims :: ResourceFields -> (ST.Text, ST.Text, Maybe ST.Text)
extractPrims (ResourceFields (ResourceName name) (ResourceEmail email) opt) =
    (name, SE.decodeUtf8 (toBytes email), a opt)
  where
    a (Just (ResourceOptional x)) = Just x
    a Nothing                     = Nothing
