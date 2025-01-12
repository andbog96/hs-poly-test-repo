module Part3.Tasks where

import Data.List(sort, group, maximumBy)
import Data.Ord(comparing)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [f(x) | x <- [n..]]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = read $ [head $ maximumBy (comparing length) digits]
    where
        digits = group $ sort $ concatMap (show . abs) nums

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x : xs) = (x : (uniq $ filter (\e -> e /= x) xs))

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f [] = []
grokBy f (h : t) = ((x, (h : filter (\e -> f e == x) t)) : grokBy f (filter (\e -> f e /= x) t))
    where x = f h
