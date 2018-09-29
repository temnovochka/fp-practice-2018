module Task1_2 where

import Todo(todo)

factorial x = product [1..x]
sumElem x n m = (-1)**n * x**(m+1) / factorial (m+1)
formula x n max s flag | n > max = s
                       | otherwise = formula x (n+1) max (s + sumElem x n (if flag then (2*n) else (2*n-1))) flag

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = formula x 0 30 0 True

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = formula x 0 30 0 False

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer -> Integer
gcd' x y k | k == 0 = y
           | otherwise = gcd' y k (mod x y)
gcd x y = gcd' x y (mod x y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool

isqrt :: Integer -> Double
isqrt = sqrt.fromIntegral

isInteger :: Double -> Bool
isInteger n = floor n == ceiling n

doesSquareBetweenExist from to | isInteger (isqrt from) == True = True
                               | isqrt to - isqrt from >= 1 = True
                               | ((fromIntegral.floor.isqrt) to > isqrt from) && (isInteger (isqrt to) == False) = True
                               | otherwise = (floor.isqrt) to - (ceiling.isqrt) from >= 1

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y | y == 0 = 1
        | mod y 2 == 1 = (pow x (y-1)) * x
        | otherwise = let k = (pow x (div y 2)) in k * k


-- является ли данное число простым?
isPrime' :: Integer -> Integer -> Bool
isPrime' x n | x < 2 = False
             | fromIntegral n > isqrt x = True
             | mod x n == 0 = False
             | otherwise = isPrime' x (n+1)
isPrime x = isPrime' x 2


type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
iks = map fst
ygrek = map snd

someMagic :: Point2D -> Double
someMagic pair = fst pair * snd pair

magicSum :: [Point2D] -> Double
magicSum = sum.map someMagic

sum1 points = magicSum (zip (iks points) ((tail.ygrek) points))
sum2 points = magicSum (zip ((tail.iks) points) (ygrek points))

shapeArea points = 0.5 * abs (sum1 points + (snd.head) points * (fst.last) points - sum2 points - (fst.head) points * (snd.last) points)

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
