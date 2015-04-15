

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




