import Data.Char

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'
  
int2letL :: Int -> Char
int2letL n = chr (ord 'a' + n)

int2letU :: Int -> Char
int2letU n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2letL ((let2int c + n) `mod` 26)
  | isUpper c = int2letU ((let2int c + n) `mod` 26)
  | otherwise = c
  
encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
