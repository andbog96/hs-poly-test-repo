{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map (Map, empty, insert, fromList, findWithDefault)
import Data.Maybe (fromMaybe)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       initMatrix :: Int -> Int -> [((Int, Int), Int)] -> mx
       element :: mx -> (Int, Int) -> Int
       dim :: mx -> (Int, Int)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       initMatrix _ _ [((0, 0), e)] = e
       initMatrix _ _ _ = 0
       element m _ = m
       dim _ = (1, 1)

instance Matrix [[Int]] where
       initMatrix w h elems = [[elem i j | i <- [0..w - 1]] | j <- [0..h - 1]]
              where elem i j = fromMaybe 0 (lookup (i, j) elems)
       element m (x, y) = (m !! y) !! x
       dim [] = (0, 0)
       dim m@(h : t) = (length h, length m)

instance Matrix (SparseMatrix Int) where
       initMatrix w h elems = SparseMatrix {
              sparseMatrixWidth = w,
              sparseMatrixHeight = h,
              sparseMatrixElements = fromList elems
       }
       dim m = (sparseMatrixWidth m, sparseMatrixHeight m)
       element m pos = findWithDefault 0 pos (sparseMatrixElements m) 

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = initMatrix w w [((i, i), 1) | i <- [0..w - 1]]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = initMatrix w h []

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix lhs rhs | w1 == h2 = 
       initMatrix w2 h1 [((j, i), sum [element lhs (k, i) * element rhs (j, k) | k <- [0..w1 - 1]])
              | i <- [0..h1 - 1], j <- [0..w2 - 1]]
              where 
                     (w1, h1) = dim lhs
                     (w2, h2) = dim rhs
              

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m | w == h = recur (toLists m)
       where
              (w, h) = dim m
              toLists mx = [[element mx (i, j) | j <- [0..w - 1]] | i <- [0..h - 1]]
              recur [[x]] = x
              recur mx@(r0 : _) = sum [(-1) ^ j * (r0 !! j) * recur (minor mx j)
                     | j <- [0..length r0 - 1]]
              minor mx@(mh : _) column = [[element (mx :: [[Int]]) (j, i) | j <- [0..length (mh) - 1], j /= column]
                     | i <- [1..length mx - 1]]
