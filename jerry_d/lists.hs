replicate :: Int -> a -> [a]
replicate n a = [a | _ <- [1 .. n]]

factors n = [x | x <- [1..n], n `mod` x == 0]

perfects n = [x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x ^ 2 + y ^ 2 == z ^ 2]

riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

divisors :: Int -> [Int]
divisors x = [d | d <- [1 .. x], x `divides` d]
