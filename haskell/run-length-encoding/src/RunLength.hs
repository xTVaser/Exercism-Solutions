module RunLength (decode, encode) where

import Data.Char

encode :: String -> String
encode []  = ""
encode str =
    if encodingLength > 1
       then show encodingLength ++ head str : encode (drop encodingLength str)
    else
        head str : encode (drop encodingLength str)
    where encodingLength = length (takeWhile (== head str) str)

-- With replicate list2 list1
-- zipWith replicate list2 list1
-- Make a seperate function to add in 1s to the string
decode :: String -> String
decode [] = ""
decode str =
    if isNumber (head str) == False
       then head str : decode (tail str)
    else
        replicate (read (takeWhile (isNumber) str)) (head strNumRemoved) ++ decode (tail strNumRemoved)
            where strNumRemoved = dropWhile (isNumber) str
