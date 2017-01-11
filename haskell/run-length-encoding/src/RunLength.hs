module RunLength (decode, encode) where

encode :: String -> String
encode []  = ""
encode str =
    if encodingLength > 1
       then show encodingLength ++ head str : decode (drop encodingLength str)
    else
        head str : decode (drop encodingLength str)
    where encodingLength = length (takeWhile (== head str) str)

-- With replicate list2 list1
-- zipWith replicate list2 list1
-- Make a seperate function to add in 1s to the string
decode :: String -> String
decode [] = ""
decode str =
