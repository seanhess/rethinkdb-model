{-# LANGUAGE OverloadedStrings #-}
module Database.RethinkDB.RethinkIO where

import Data.Text (Text)
import Database.RethinkDB.NoClash
import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)

        
-- Database Helpers ----------------------------------------

-- a connected database, handle, and resource
data Connection = Connection RethinkDBHandle Database
type RethinkIO = ReaderT Connection IO
        
connectDb :: String -> Text -> IO Connection
connectDb dbHost dbName = do
    hand <- connect dbHost 28015 Nothing
    let database = db dbName
    let handleDb = use database hand
    -- run' connection $ dbCreate "courses"
    return $ Connection handleDb database

runDb :: (Expr a, Result r) => a -> RethinkIO r
runDb e = do
    Connection h _ <- ask
    liftIO $ run h e
