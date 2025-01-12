module Part1.Tasks where

import Data.Fixed (mod')

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum $ takeWhile (\a -> abs a > 1e-10) [
        ((-1) ^ k * (mod' x (2 * pi)) ^ (2 * k + 1)) / fromIntegral (factorial (2 * k + 1)) 
        | k <- [0..]
    ]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum $ takeWhile (\a -> abs a > 1e-10) [
        ((-1) ^ k * (mod' x (2 * pi)) ^ (2 * k)) / fromIntegral (factorial (2 * k)) 
        | k <- [0..]
    ]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b 
    | b == 0 = abs a
    | otherwise = myGCD b (mod a b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = year >= 0 && month >= 1 && month <= 12 && day >= 1 && day <= (daysInMonth year month)
    where 
        daysInMonth year month
            | month == 2 = if isLeapYear year then 29 else 28
            | elem month [4, 6, 9, 11] = 30
            | otherwise = 31
            where
                isLeapYear year = (mod year 4 == 0 && mod year 100 /= 0) || (mod year 400 == 0)

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a 0 = 1
myPow a 1 = a
myPow a b = a * (myPow a (b - 1))  

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime p = helper p (p - 1)
    where
        helper p 1 = True
        helper p q 
            | p `mod` q == 0 = False
            | otherwise = helper p (q - 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points@(x:xs) = abs $ sum deltas / 2
     where
        deltas = [x1 * y2 - x2 * y1| ((x1, y1), (x2, y2)) <- zip points (xs ++ [x])]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c 
    | a + b <= c || a + c <= b || b + c <= a = -1
    | a2b2 == c2 || a2c2 == b2 || b2 + c2 == a2 = 2
    | a2b2 <  c2 || a2c2 <  b2 || b2 + c2 <  a2 = 0
    | a2b2 >  c2 && a2c2 >  b2 && b2 + c2 >  a2 = 1
    where 
        a2 = a * a
        b2 = b * b
        c2 = c * c
        a2b2 = a2 + b2
        a2c2 = a2 + c2
        b2c2 = b2 + c2
