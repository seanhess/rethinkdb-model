{-# LANGUAGE OverloadedStrings #-}

module Database.RethinkDB.Model.IO where

import Data.Text (Text)
import Database.RethinkDB.NoClash

import Control.Applicative ((<$>))
import Control.Monad.Trans.Reader
import Control.Monad.Trans (liftIO)


-- RethinkIO ---------------------------------------------

runDb :: (Expr a, Result r) => a -> RethinkIO r
runDb e = do
    h <- ask
    liftIO $ run h e

type RethinkIO = ReaderT RethinkDBHandle IO
