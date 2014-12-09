{-# LANGUAGE OverloadedStrings #-}
module Database.RethinkDB.ReIO where

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)

        
-- Database Helpers ----------------------------------------

-- a connected database, handle, and resource
data Connection = Connection RethinkDBHandle Database
type RethinkIO = ReaderT Connection IO


type Host = String
type Port = Integer
data Endpoint = Endpoint Host Port deriving (Show, Eq)

instance Read Endpoint where

-- pass in something like: localhost:28015
connectDb :: Endpoint -> Text -> IO Connection
connectDb (Endpoint host port) dbName = do
    hand <- connect host port Nothing
    let database = db dbName
    let handleDb = use database hand
    -- run' connection $ dbCreate "courses"
    return $ Connection handleDb database

readEndpoint :: String -> Endpoint
readEndpoint s = Endpoint host port
    where
    (host, rest) = span (/= ':') s
    port = read $ drop 1 rest

runDb :: (Expr a, Result r) => a -> RethinkIO r
runDb e = do
    Connection h _ <- ask
    liftIO $ run h e
