{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Model.HasId where

import Control.Monad (mzero)

import Data.Aeson
import Database.RethinkDB hiding (Object, toJSON)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)

-- * Id field

-- | Use HasId to control how the id field is handled for your records. 
-- For example, if I have the following record:
-- 
-- > data User = { username :: Text } deriving (Show, Generic)
-- > instance ToJSON User
-- > instance FromJSON User
--
-- This will be stored as @{ id: "...", username: "..." }@ by rethinkdb. Now, we can write functions that include the id:
--
-- > getUser :: Id -> IO (Maybe (HasId User))
-- > getUser id = run h $ table "users" # get (expr id)
-- 
-- Or if you don't want to include the id, just leave off @HasId@
--
-- > addUser :: User -> IO ()
-- > addUser user = run h $ table "users" # insert (toDatum user)

data HasId a = HasId Id a deriving (Show)

type Id = Text

instance ToJSON a => ToJSON (HasId a) where
    toJSON (HasId i item) = object $ ("id" .= i) : pairs (toJSON item)
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


