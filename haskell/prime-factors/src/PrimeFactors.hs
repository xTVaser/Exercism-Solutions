module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors num
    | isPrime num = [num]
    | otherwise = firstPrime : primeFactors (num `quot` firstPrime)
                    where firstPrime = head [ x | x <- primeList, num `mod` x == 0 ]

primeList = [ x | x <- [2..], isPrime x]

isPrime :: Integer -> Bool
isPrime num
   | num == 2 = True
   | length [ x | x <- [2..ceiling (sqrt $ fromIntegral(num))], num `mod` x == 0 ] > 0 = False
   | otherwise = True
