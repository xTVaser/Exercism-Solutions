module Raindrops (convert) where

convert :: Integer -> String
convert n =
    if result == ""
       then show n
    else
        result
    where result = factorThree n ++ factorFive n ++ factorSeven n


factorThree :: Integer -> String
factorThree n =
    if n `mod` 3 == 0 then "Pling" else ""

factorFive :: Integer -> String
factorFive n =
    if n `mod` 5 == 0 then "Plang" else ""

factorSeven :: Integer -> String
factorSeven n =
    if n `mod` 7 == 0 then "Plong" else ""
