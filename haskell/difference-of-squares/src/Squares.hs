module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference lim = squareOfSums lim - sumOfSquares lim

squareOfSums :: Integral a => a -> a
squareOfSums lim = sum [1..lim]^2

sumOfSquares :: Integral a => a -> a
sumOfSquares lim = sum[ x^2 | x <- [1..lim] ]
