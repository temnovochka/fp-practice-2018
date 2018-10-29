module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldr :: (a -> b -> b) -> b -> [a] -> b

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons RNil a) = a : []
rlistToList (RCons r a) = (rlistToList r) ++ [a]

makeReverse :: ReverseList a -> a -> ReverseList a
makeReverse rl l = RCons rl l

listToRList :: [a] -> ReverseList a
listToRList [] = RNil
listToRList lst = foldl makeReverse RNil lst

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

magicShow (RCons RNil a) = show a
magicShow (RCons r a) = show a ++ ", " ++ magicShow r

instance Show a => Show (ReverseList a) where
    show RNil = "[]"
    show rl = "[" ++ magicShow rl ++ "]"

instance Eq a => Eq (ReverseList a) where
    (==) RNil RNil = True
    (==) RNil _ = False
    (==) _ RNil = False
    (==) (RCons ra a) (RCons rb b)
        | a == b = (==) ra rb
        | otherwise = False

instance Ord a => Ord (ReverseList a) where
    (<=) RNil RNil = True
    (<=) RNil _ = True
    (<=) _ RNil = False
    (<=) (RCons ra a) (RCons rb b)
        | a <= b = (<=) ra rb
        | otherwise = False

plus :: ReverseList a -> ReverseList a -> ReverseList a
plus RNil res = res
plus (RCons ra a) res = plus ra (RCons res a)

instance Semigroup (ReverseList a) where
    (<>) a b = mappend a b

instance Monoid (ReverseList a) where
    mempty = RNil

    mappend RNil RNil = RNil
    mappend RNil a = a
    mappend a RNil = a
    mappend a b = plus (plus a RNil) b

instance Functor ReverseList where
    fmap f RNil = RNil
    fmap f (RCons ra a) = RCons (fmap f ra) (f a)
