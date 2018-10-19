module Task2_1 where

import Todo(todo)

import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Pusto
               | Both Integer v (TreeMap v) (TreeMap v)
               deriving(Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Pusto

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool

contains' Pusto _ = False
contains' (Both key _ l r) k | k == key = True
                             | k > key = contains' r k
                             | k < key = contains' l k

contains t k = contains' t k 

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v

lookup' Pusto _ = error "404 not found"
lookup' (Both key value l r) k | k == key = value
                               | k > key = lookup' r k
                               | k < key = lookup' l k

lookup k t = lookup' t k

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v

insert' Pusto (k, v) = Both k v Pusto Pusto
insert' (Both key value l r) (k, v) | k == key = Both k v l r
                                    | k > key = Both key value l (insert' r (k, v))
                                    | k < key = Both key value (insert' l (k, v)) r

insert (k, v) t = insert' t (k, v)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v

insertIntoLeft Pusto lt = lt
insertIntoLeft (Both k v Pusto r) lt = Both k v lt r
insertIntoLeft (Both k v l r) lt = Both k v (insertIntoLeft l lt) r

remove' Pusto _ = Pusto
remove' (Both k v l r) i | i == k = insertIntoLeft r l
                         | i > k = Both k v l (remove' r i)
                         | i < k = Both k v (remove' l i) r

remove i t = remove' t i

nearestLE' :: TreeMap a -> Integer -> (Integer, a)
nearestLE' Pusto _ = error "404 not found"
nearestLE' (Both key value l r) i 
    | i == key = (key, value)
    | i < key = nearestLE' l i
    | i > key = case r of (Both k v _ _) | (i == k) -> (k, v)
                          (Both k v _ _) | (i /= k) -> nearestLE' r i
                          otherwise -> (key, value)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = nearestLE' t i

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Pusto lst

--listFromTree' :: TreeMap v -> [(Integer, v)]
listFromTree' Pusto = []
listFromTree' (Both k v Pusto Pusto) = [(k, v)]
listFromTree' (Both k v l r) = (listFromTree' l) ++ [(k, v)] ++ (listFromTree' r)

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = listFromTree' t

magicCiferka _ Pusto n = error "404 not found ciferka"
magicCiferka _ (Both k v Pusto Pusto) n = n
magicCiferka m (Both k v Pusto r) n 
    | m == k = n
    | otherwise = magicCiferka m r (n + 1)
magicCiferka m (Both k v l Pusto) n = magicCiferka m l (n + 1)
magicCiferka m (Both k v l r) n 
    | m == k = magicCiferka m l (n + 1)
    | otherwise = (magicCiferka m l 1) + (magicCiferka m r 1) + n

specialEditionMagic Pusto _ _ = error "404 not found tree"
specialEditionMagic t @(Both k v l r) i mn
    | i == m = (k, v)
    | i > m = case r of Pusto -> error "404 not found right"
                        otherwise -> specialEditionMagic r i (m + 1)
    | i < m = specialEditionMagic l i mn
    where m = magicCiferka k t mn

kMeanCheck i t = (listFromTree t) !! (fromInteger i)

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = specialEditionMagic t i 0
