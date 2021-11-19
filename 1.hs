{-============================================================================================
    1.
-}
-- (a)

index_a :: Eq a => a -> [a] -> Maybe Int
index_a elem []    = Nothing
index_a elem (x:xs)
    | elem == x  = Just 0
    | otherwise  = fmap (+1) (index_a elem xs)

----------------------------------------------------------------------------------------------
-- (b)

index_b :: Eq a => a -> [a] -> Maybe Int
index_b elem []    = Nothing
index_b elem (x:xs)
    | elem == x  = Just 0
    | otherwise  = (pure (+1)) <*> (index_b elem xs)

----------------------------------------------------------------------------------------------
-- (c)

index_c :: Eq a => a -> [a] -> (Int -> Int) -> Maybe Int
index_c elem [] k  = Nothing
index_c elem (x:xs) k
    | elem == x  = Just (k 0)
    | otherwise  = index_c elem xs (\v->k(1+v))

----------------------------------------------------------------------------------------------
-- (d)

newtype K r a = K ((a -> r) -> r)

(<<<) :: K r a -> (a -> r) -> r
(K f) <<< k = f k

instance Monad (K r) where
    return v = K(\k->k v)
    m >>= f  = K(\k->m <<< (\a->(f a) <<< k))

instance Applicative (K r) where
    pure  = return
    mf <*> ma = do f <- mf
                   a <- ma
                   return (f a)

instance Functor (K r) where
  fmap g fx = (pure g) <*> fx

abortWith :: Maybe Integer -> K (Maybe Integer) (Maybe Integer)
abortWith v = K(\k->v)


add :: Num b => Maybe b -> Maybe b
add mx = mx >>= (\x -> Just $ x+1)

index_d :: Eq a => a -> [a] ->  K (Maybe Integer) (Maybe Integer)
index_d elem []     = abortWith Nothing
index_d elem (x:xs)
    | elem == x    =  return (Just 0)
    | otherwise    =  do 
        v1 <- (index_d elem xs)
        return (add v1)

topIndex_d :: Eq a => a -> [a] -> Maybe Integer
topIndex_d elem xs = (index_d elem xs) <<< id

----------------------------------------------------------------------------------------------
-- (e)

index_e :: Eq a => a -> [a] -> Maybe Int
index_e elem []      = Nothing
index_e elem (x:xs)
    | x == elem     = Just 0
    | otherwise     = (index_e elem xs) >>= (\index->Just (index+1))

----------------------------------------------------------------------------------------------
-- (f) 

index_f :: Eq a => a -> [a] -> Maybe Int
index_f elem []      = Nothing
index_f elem (x:xs)
    | x == elem     = Just 0
    | otherwise     = do index <- (index_f elem xs); return (index + 1)
----------------------------------------------------------------------------------------------