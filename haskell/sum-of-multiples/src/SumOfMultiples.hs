module SumOfMultiples (sumOfMultiples) where

import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub [ x | x <- [1..limit-1], y <- factors, x `mod` y == 0 ]

