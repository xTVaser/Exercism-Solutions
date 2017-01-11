module Bob (responseFor) where

import Data.Char
import Data.Strings

checkForLetter :: String -> Bool
checkForLetter [] = False
checkForLetter str
    | isAlpha (head str) = True
    | otherwise = checkForLetter (tail str)

responseFor :: String -> String
responseFor str
    | null (strTrim str) = "Fine. Be that way!"
    | str == map toUpper str && checkForLetter str == True = "Whoa, chill out!"
    | last (strTrim str) == '?' = "Sure."
    | otherwise = "Whatever."
