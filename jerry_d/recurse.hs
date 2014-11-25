
repl :: Int -> a -> [a]
repl 0 _ = []
repl n x = x: repl (n - 1) x

bang :: [a] -> Int -> a  
(x : _) `bang` 0 = x
(_ : xs) `bang` n = xs !! (n - 1)

element :: Eq a => a -> [a] -> Bool
element _ [] = False
element x (y : ys)
  | x == y = True
  | otherwise = element x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x <= y then x : merge xs (y : ys) else y : merge (x : xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
  where (ys, zs) = halve xs

aaand :: [Bool] -> Bool
aaand [] = True
aaand (b : bs) = b && aaand bs

--aaand (b : bs)
--  | b = and bs
--  | otherwise = False

--aaand (b : bs)
--  | b == False = False
--  | otherwise = and bs

--aaand (b : bs) = and bs && b
