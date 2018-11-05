module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

normalize :: Double -> Double
normalize x | x >= (-pi/2) && x <= (pi/2) = x
            | x > (pi/2) && x < (3*pi/2) = pi-x
            | x >= (3*pi/2) = normalize $ x-2*pi
            | otherwise = normalize $ x+2*pi

factorial x = product [1..x]

sinPrecisions' x n sum = Cons elem $ sinPrecisions' x (n+1) elem
    where elem = sum+(-1)**n*x**(2*n+1)/factorial (2*n+1)

ePrecisions' n sum = Cons elem $ ePrecisions' (n+1) elem
    where elem = sum+1/factorial n

sinPrecisions :: Double -> Stream Double
sinPrecisions x = sinPrecisions' (normalize x) 0 0

ePrecisions :: Stream Rational
ePrecisions = ePrecisions' 0 0
