module Api.Abstract
( (++?)
) where

import Data.Monoid

(++?) :: (Monoid a) => a -> Maybe a -> Maybe a
a ++? (Just b) = Just (a <> b)
_ ++? Nothing  = Nothing
