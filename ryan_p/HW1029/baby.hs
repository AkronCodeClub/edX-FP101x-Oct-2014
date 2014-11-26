
double x = x + x
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
                 where
                   smaller = [a | a <- xs, a <= x]
                   larger = [b | b <- xs, b > x]
				  
mysum [] = 0
mysum (x : xs) = x + mysum xs

myproduct [] = 1
myproduct (x : xs) = x * myproduct xs

aqsort [] = []
aqsort (x : xs) = aqsort larger ++ [x] ++ aqsort smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

aqsort' [] = []
aqsort' (x : xs) = aqsort' larger ++ [x] ++ aqsort' smaller
  where smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]

eqsort [] = []
eqsort (x : xs) = eqsort larger ++ [x] ++ eqsort smaller
  where larger = [a | a <- xs, a > x || a == x]
        smaller = [b | b <- xs, b < x]
		
gqsort [] = []
gqsort (x :xs)
  = reverse
      (reverse (gqsort smaller) ++ [x] ++ reverse (gqsort larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
		
second xs = head (tail xs)

swap (a,b) = (b,a)

pair x y = (x, y)

palindrome xs = reverse xs == xs

twice f x = f (f x)

f xs = take 3 (reverse xs)

factors n = [x | x <- [1..n], n `mod` x == 0]

fix f = f (fix f)

-- halve1 xs = (take n xs, drop n xs)
--   where n = length xs / 2
  
halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
  
halve4 xs = splitAt (length xs `div` 2)

halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2
  
halve6 xs = splitAt (div (length xs) 2) xs

-- halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2
  
safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs
  
safetail4 xs
  | null xs = []
  | otherwise = tail xs
  
--safetail5 xs = tail xs
--safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

safetail7 [x] = [x]
safetail7 (_ : xs) = xs

--safetail8
--  = \ xs ->
--      case xs of
--	      [] -> []
--		  (_ : xs) -> xs

--import Prelude hiding ((||))

bc2 a b = if a then if b then True else False else False

bc3 a b = if not (a) then not (b) else True

--bc4 a b = if a then b

bc1 a b
  | not (a) == b = False
  | otherwise = True

mult1 x y z = \ x -> (\ y -> (\ z -> x * y * z)) -- doesn't work

--mult2 = \ x -> (x * \ y -> (y * \ z -> z))

mult3 = \ x -> (\ y -> (\ z -> x * y * z))

--mult4 = ((((\x -> \y) -> \z) -> x * y) * z)

--remove1 n xs = take n xs ++ drop n xs --ng

--remove2 n xs = drop n xs ++ take n xs --ng

--remove3 n xs = take (n + 1) xs ++ drop n xs --ng

remove4 n xs = take n xs ++ drop (n + 1) xs

exercise8 :: Int -> [a] -> [a]
exercise8 x xs = take (x + 1) xs ++ drop x xs

perfects2 n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num
  
e4 = [(x,y) | x <- [1,2,3], y <- [4,5,6]]

find :: (Eq a) => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..n], x == x']
  where n = length xs - 1

positions1 :: (Eq a) => a -> [a] -> [Int]
positions1 x xs = find x (zip xs [0..n])
  where n = length xs - 1

--scalarproduct :: [Int] -> [Int] -> Int
--scalarproduct xs ys = sum (product [(x,y) | x <- xs, y <- ys])

init1 :: [a] -> [a]
init1 [_] =[]
init1 (x : xs) = x : init1 xs

and1 :: [Bool] -> Bool
and1 [] = True
and1 (b : bs)
  | b = b
  | otherwise = and1 bs
  
--concat1 :: [[a]] -> [a]
--concat1 [] = []
--concat1 (xs : xss) = xs : concat1 xss

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (xs : xss) = xs ++ concat2 xss

--concat3 :: [[a]] -> [a]
--concat3 [] = [[]]
--concat3 (xs : xss) = xs ++ concat3 xss

concat4 :: [[a]] -> [a]
concat4 [[]] = []
concat4 (xs: xss) = xs ++ concat4 xss


  

  

  

