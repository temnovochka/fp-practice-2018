module Task1_2 where

import Todo(todo)

factorial x = product [1..x]
teylorParam f n = if f then (2*n+1) else (2*n)
sumElem x n m = (-1)**n * x**m / factorial m
formula x n eps nterm s flag | abs (nterm) <= eps = s
                             | otherwise = formula x (n+1) eps (sumElem x n (teylorParam flag n)) (s + nterm) flag

normalize :: Double -> Double -> Double
normParam s = if s < 0 then 1 else 2
normalize x s | x >= s && x <= s+pi = x
              | x > s+pi && x < s+2*pi = normParam s * pi - x
              | x >= s+2*pi = normalize (x - 2*pi) s
              | otherwise = normalize (x + 2*pi) s -- x < s

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = formula (normalize x (-pi/2)) 0 1e-20 1 (-1) True

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = formula (normalize x 0) 0 1e-20 1 (-1) False

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
term1 points = sum1 points - sum2 points
term2 points = (snd.head) points * (fst.last) points
term3 points = (fst.head) points * (snd.last) points

shapeArea points = 0.5 * abs (term1 points + term2 points - term3 points)

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
