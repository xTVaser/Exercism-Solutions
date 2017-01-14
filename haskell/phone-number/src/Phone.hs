module Phone (areaCode, number, prettyPrint) where

import Data.Char

areaCode :: String -> Maybe String
areaCode str
    | length number < 10 = Nothing
    | otherwise = Just $ take 3 number
    where number = filter isDigit str

number :: String -> Maybe String
number str
    | length number < 10 || length number > 11 = Nothing
    | length number == 11 && head number == '1' = Just $ tail number
    | length number == 11 && head number /= '1' = Nothing
    | otherwise = Just $ number
    where number = filter isDigit str

prettyPrint :: String -> Maybe String
prettyPrint str
    | length number < 10 = Nothing
    | length number == 11 && head number == '1'
    = Just ("(" ++ take 3 numberOneRemoved ++ ") " ++ fst halfNumRemoved ++ "-" ++ snd halfNumRemoved)
    | otherwise = Just ("(" ++ take 3 number ++ ") " ++ fst halfNum ++ "-" ++ snd halfNum)
    where number = filter isDigit str
          numberOneRemoved = drop 1 number
          halfNum = splitAt 3 $ drop 3 number
          halfNumRemoved = splitAt 3 $ drop 3 numberOneRemoved
