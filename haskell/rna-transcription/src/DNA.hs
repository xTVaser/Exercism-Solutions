module DNA (toRNA) where

import Data.List

toRNA :: String -> Maybe String
toRNA [] = Nothing
toRNA str = if find (\c -> c /= 'G'
                           && c /= 'C'
                           && c /= 'T'
                           && c /= 'A') str /= Nothing
            then Nothing
            else Just (concatMap (\c -> if c == 'G' then "C"
                            else if c ==  'C' then "G"
                            else if c == 'T' then "A"
                            else if c == 'A' then "U"
                            else [c]) str)
