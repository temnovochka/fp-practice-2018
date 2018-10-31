module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor (FourOf) where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative (FourOf) where
    pure = return
    f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

first  (FourOf a _ _ _) = a
second (FourOf _ a _ _) = a
third  (FourOf _ _ a _) = a
fourth (FourOf _ _ _ a) = a

instance Monad (FourOf) where
    (FourOf a b c d) >>= f = 
        FourOf (first (f a)) (second (f b)) (third (f c)) (fourth (f d))
    return a = FourOf a a a a
