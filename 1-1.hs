import Data.Char (ord)

-- 48 to 57
isDigit :: Char -> Bool
isDigit x = 48 <= y && y < 58
    where y = ord x


firstDigit :: [Char] -> Int
firstDigit [] = 0
firstDigit (x:xs) = if isDigit x then ord x - 48 else firstDigit xs

lastDigit :: [Char] -> Int
lastDigit list = firstDigit (reverse list)


main = do
    content <- lines <$> readFile "1-1.txt"
    print ((sum (map firstDigit content) * 10) + (sum (map lastDigit content)))
