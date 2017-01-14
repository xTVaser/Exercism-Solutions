module Hamming (distance) where

distance :: String -> String -> Maybe Integer
distance str1 str2
    | length str1 /= length str2 = Nothing
    | str1 == "" && str2 == "" = Just 0
    | str1 == "" || str2 == "" = Nothing
    | otherwise = Just $ toInteger $ length . filter isDiff $ zip str1 str2

isDiff n = fst n /= snd n
