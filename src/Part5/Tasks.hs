module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f e [] = e
myFoldl f e (h : t) = myFoldl f (f e h) t 

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f e [] = e
myFoldr f e (h : t) = f h (myFoldr f e t)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldl (\curr e -> curr ++ [f e]) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldl (\curr e -> curr ++ (f e)) [] 

myConcat :: [[a]] -> [a]
myConcat = myFoldl (\curr e -> curr ++ e) []

myReverse :: [a] -> [a]
myReverse = myFoldl (\curr e -> (e : curr)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldl (\curr e -> if (p e) then curr ++ [e] else curr) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldl f ([], [])
    where 
        f c e = if (p e)
            then (fst c ++ [e], snd c)
            else (fst c, snd c ++ [e])
