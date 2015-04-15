-- | Haskell client tools for working with RethinkDB models
--
-- Please learn to the 'Database.RethinkDB' driver first
--

module Database.RethinkDB.Model (
  -- ** Ids
  HasId(..), Id,
  insertKey, create, setId,
  -- ** IO
  RethinkIO, runDb

) where

import Database.RethinkDB.Model.HasId
import Database.RethinkDB.Model.IO

