import Data.Char

--let2int :: Char -> Int
--let2int c = ord c - ord 'a'
--
--int2let :: Int -> Char
--int2let n = chr (ord 'a' + n)
--
--shift :: Int -> Char -> Char
--shift n c
--  | isLower c = int2let ((let2int c + n) `mod` 26)
--  | otherwise = c
--
--encode :: Int -> String -> String
--encode n xs = [shift n x | x <- xs]

let2int :: Char -> Int
let2int c = ord c - ord 'a'

cap2int :: Char -> Int
cap2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2cap :: Int -> Char
int2cap n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | isUpper c = int2cap ((cap2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
