{-# LANGUAGE OverloadedStrings #-}
module Database.RethinkDB.ReTools where

import Prelude hiding ((==), (&&), not, id)
import Database.RethinkDB hiding (Object)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Control.Monad (mzero)

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

-- Deletable ---------------------------------------------
data Deletable a = Deletable Bool a deriving (Show)

-- serialize it with the deleted field
instance ToJSON a => ToJSON (Deletable a) where
    toJSON (Deletable d item) = object $ ("deleted" .= d) : pairs (toJSON item)
        where pairs (Object o) = H.toList o
              pairs _ = []

instance FromJSON a => FromJSON (Deletable a) where
    parseJSON (Object v) = do
        o <- parseJSON (Object v)
        d <- v .:? "deleted"
        return $ Deletable (fromMaybe False d) o
    parseJSON _ = mzero

instance (ToDatum a, ToJSON a) => ToDatum (Deletable a)
instance (FromDatum a, FromJSON a) => FromDatum (Deletable a)

-- marks a record as deleted
--markDeleted :: Table -> Id -> RethinkIO ()
--markDeleted t id = runDb $ update deleted (byId t id)

deleted :: b -> [Attribute a]
deleted = const ["deleted" := True]

-- filter by not deleted
-- don't use an index, just filter them out

notDeleted :: (Expr a) => a -> ReQL
notDeleted = not . isDeleted

isDeleted :: (Expr a) => a -> ReQL
isDeleted doc = hasFields "deleted" doc && (doc !? "deleted") == True




-- LIBRARY ---------------------------------------------------
-- returns the inserted key or an empty string
-- I should figure out how to make it error instead
insertKey :: WriteResponse -> Text
insertKey = head . fromMaybe [""] . writeResponseGeneratedKeys

-- ooh, works with HasId to return it with an Id! Sweet!
--create :: (ToDatum a) => Table -> a -> RethinkIO (HasId a)
--create t obj = do
    --result <- runDb $ insert (toDatum obj) t
    --return $ setId result obj
create :: (ToDatum a) => Table -> a -> ReQL
create t obj = insert (toDatum obj) t

setId :: (ToDatum a) => WriteResponse -> a -> HasId a
setId result obj = HasId (insertKey result) obj

toRQL :: Text -> ReQL
toRQL = str . unpack

byId :: Table -> Text -> ReQL
byId t id = get (toRQL id) t


