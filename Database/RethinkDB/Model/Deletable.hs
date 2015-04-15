{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Model.Deletable where

import Control.Monad (mzero)

import Prelude hiding ((==), (&&), not)

import Data.Aeson
import Database.RethinkDB hiding (Object, toJSON)
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text, unpack)

-- Deletable ---------------------------------------------

data Deletable a = Deletable Bool a deriving (Show)

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

deleted :: b -> [Attribute a]
deleted = const ["deleted" := True]

notDeleted :: (Expr a) => a -> ReQL
notDeleted = not . isDeleted

isDeleted :: (Expr a) => a -> ReQL
isDeleted doc = hasFields "deleted" doc && (doc !? "deleted") == True




