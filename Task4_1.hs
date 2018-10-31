module Task4_1 where

-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor (FunMonad) where
    fmap f FunMonad { fun = myF } = FunMonad { fun = newF } where newF = f.myF

instance Applicative (FunMonad) where
    pure = return
    f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

instance Monad (FunMonad) where
    FunMonad { fun = myF } >>= f = FunMonad { fun = newF } 
        where newF str = fun (f (myF str)) str
    return a = FunMonad { fun = f } where f str = a
