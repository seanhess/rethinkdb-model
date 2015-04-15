{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Model.HasId where

import Prelude hiding ((==), (&&), not, id)

import Control.Monad (mzero)

import Data.Aeson
import Database.RethinkDB hiding (Object)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text, unpack)

-- HasId --------------------------------------------------
-- shared code for working with id-based resources
-- this is cool because you can say whether a route is expecting
-- raw objects or objects with ids (like for create, we want to ignore the id)

type Id = Text
data HasId a = HasId Id a deriving (Show)

instance ToJSON a => ToJSON (HasId a) where
    toJSON (HasId id item) = object $ ("id" .= id) : pairs (toJSON item)
        where pairs (Object o) = H.toList o
              pairs _ = []

instance FromJSON a => FromJSON (HasId a) where
    parseJSON (Object v) = do
        o <- parseJSON (Object v)
        i <- v .: "id"
        return $ HasId i o
    parseJSON _ = mzero

instance (ToDatum a, ToJSON a) => ToDatum (HasId a)
instance (FromDatum a, FromJSON a) => FromDatum (HasId a)

-- LIBRARY ---------------------------------------------------
-- returns the inserted key or an empty string
-- I should figure out how to make it error instead

insertKey :: WriteResponse -> Text
insertKey = head . fromMaybe [""] . writeResponseGeneratedKeys

create :: (ToDatum a) => Table -> a -> ReQL
create t obj = insert (toDatum obj) t

setId :: (ToDatum a) => WriteResponse -> a -> HasId a
setId result obj = HasId (insertKey result) obj

toRQL :: Text -> ReQL
toRQL = str . unpack

byId :: Table -> Text -> ReQL
byId t id = get (toRQL id) t


