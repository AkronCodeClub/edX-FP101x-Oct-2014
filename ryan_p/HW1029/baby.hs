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
		
		