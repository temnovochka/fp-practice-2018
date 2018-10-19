module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl fun s (h:[]) = fun s h
foldl fun s (h:t) = foldl fun (fun s h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr fun s (h:[]) = fun h s
foldr fun s (h:t) = fun h (foldr fun s t)

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a] -> [a]
unfoldr' fun e lst =
    let res = fun e in
    case res of Nothing -> lst
                Just(a, b) -> unfoldr' fun b (a:lst)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr fun e = reverse (unfoldr' fun e [])

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map fun lst = foldr (f fun) [] lst where f fun h t = (fun h):t

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst | lst == [] = 0
            | otherwise = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes lst = foldr f [] lst where f h r = case h of Nothing -> r
                                                       Just a -> a:r

luuuuuu :: [a] -> Integer
luuuuuu lst = foldl len 0 lst where len n _ = 1 + n

magicH :: (Integer, [a]) -> [a] -> (Integer, [a])
magicH (n, res) lst 
    | (luuuuuu lst) <= n = (n, res)
    | otherwise = ((n + 1), res ++ [lst !! (fromInteger n)])

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal m = snd (foldl magicH (0, []) m)

dontLike f a res = case (f a) of True -> res
                                 False -> a:res

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f lst = foldr (dontLike f) [] lst

poisk e lst r | lst == e = True
              | otherwise = r

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = foldr (poisk e) False lst

magicAwesomeRange step to from | from >= to = Nothing
                               | otherwise = Just(from, (from + step))

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (magicAwesomeRange step to) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a [] = a
append a b = foldr fall_down b a where fall_down a b = a:b

aLotOfMagic _ ([], 0) l = ([[l]], 1)
aLotOfMagic n ((h:t), numelem) l | numelem < n = ((append h [l]):t, (numelem + 1))
                                 | otherwise = ([l]:h:t, 1)

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = reverse $ fst(foldl (aLotOfMagic n) ([], 0) lst)
