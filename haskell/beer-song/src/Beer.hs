module Beer (song) where

song :: String
song = concatMap beer [99, 98 .. 0]

beer :: Integer -> String
beer 2 = "2 bottles of beer on the wall, 2 bottles of beer.\nTake one down and pass it around, 1 bottle of beer on the wall.\n\n"
beer 1 = "1 bottle of beer on the wall, 1 bottle of beer.\nTake it down and pass it around, no more bottles of beer on the wall.\n\n"
beer 0 = "No more bottles of beer on the wall, no more bottles of beer.\nGo to the store and buy some more, 99 bottles of beer on the wall.\n"
beer n = show n ++ " bottles of beer on the wall, "
                ++ show n
                ++ " bottles of beer.\nTake one down and pass it around, "
                ++ show (n-1)
                ++ " bottles of beer on the wall.\n\n"
