trues = [True, True, True, True]
falses = [False, False, False, False]
mixed = [True, False, True, False]

ok :: Bool -> Bool
ok x = x == True

ifAll :: (a -> Bool) -> [a] -> Bool

--ifAll p xs = and (map p xs)
--ifAll p = and . map p
--ifAll p = not . any (not . p)
--ifAll p xs = foldl (&&) True (map p xs)
ifAll p = foldr (&&) True . map p

ifAny :: (a -> Bool) -> [a] -> Bool

--ifAny p = or . map p
--ifAny p xs = length (filter p xs) > 0
--ifAny p = not . null . dropWhile (not . p)
--ifAny p xs = not (all (\ x -> not (p x)) xs)
ifAny p xs = foldr (\ x acc -> (p x) || acc) False xs

take_while :: (a -> Bool) -> [a] -> [a]
take_while _ [] = []
take_while p (x : xs)
  | p x = x : take_while p xs
  | otherwise = []

--take_while (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--take_while (< 9) [1,2,3] == [1,2,3]
--take_while (< 0) [1,2,3] == [] 

drop_while :: (a -> Bool) -> [a] -> [a]
drop_while _ [] = []
drop_while p (x : xs)
  | p x = drop_while p xs
  | otherwise = x : xs

--drop_while (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
--drop_while (< 9) [1,2,3] == []
--drop_while (< 0) [1,2,3] == [1,2,3] 

mapper :: (a -> b) -> [a] -> [b]
mapper f = foldl (\ xs x -> xs ++ [f x]) []

--mapper (+3) [1,5,3,1,6] = [4,8,6,4,9]  
--mapper (++ "!") ["BIFF", "BANG", "POW"] = ["BIFF!","BANG!","POW!"]  
--mapper (replicate 3) [3..6] = [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
--mapper (mapper (^2)) [[1,2],[3,4,5,6],[7,8]] = [[1,4],[9,16,25,36],[49,64]]  
--mapper fst [(1,2),(3,5),(6,3),(2,6),(2,5)] = [1,3,6,2,2]  

filterer :: (a -> Bool) -> [a] -> [a]
filterer p = foldr (\ x xs -> if p x then x : xs else xs) []

--filterer (>3) [1,5,3,2,1,6,4,3,2,1] = [5,6,4]  
--filterer (==3) [1,2,3,4,5] = [3]  
--filterer even [1..10] = [2,4,6,8,10]  
--filterer (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent" = "uagameasadifeent"  
--filterer (`elem` ['A'..'Z']) "i lauGh At You BecAuse u r aLL the Same" = "GAYBALLS" 

dec2int :: [Integer] -> Integer
dec2int = foldl (\ x y -> 10 * x + y) 0

--dec2int [2, 3, 4, 5] = 2345
--dec2int [] = 0
--dec2int [0, 0, 0, 0] = 0

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

sambar :: ((a, b) -> c) -> a -> b -> c
sambar f = \ x y -> f (x, y)

unsambar :: (a -> b -> c) -> (a, b) -> c
unsambar f = \ (x, y) -> f x y

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int

int2bin :: Int -> [Bit]
--int2bin 0 = []
--int2bin n = n `mod` 2 : int2bin (n `div` 2)
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

--int2bin 13 = [1, 0, 1, 1]
--int2bin (-0) = []

chop8 :: [Bit] -> [[Bit]]
--chop8 [] = []
--chop8 bits = take 8 bits : chop8 (drop 8 bits)
chop8 = unfold null (take 8) (drop 8)

--chop8 [1] = [[1]]
--chop8 [1,1,1,1,1,1,1,1] = [[1,1,1,1,1,1,1,1]]
--chop8 [1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0] = [[1,1,1,1,1,1,1,1],[0,0,0,0,0,0,0,0]]

map2 :: (a -> b) -> [a] -> [b]
map2 f = unfold null (f . head) tail

--map2 (+3) [1,5,3,1,6] = [4,8,6,4,9]  
--map2 (++ "!") ["BIFF", "BANG", "POW"] = ["BIFF!","BANG!","POW!"]  
--map2 (replicate 3) [3..6] = [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]  
--map2 (map2 (^2)) [[1,2],[3,4,5,6],[7,8]] = [[1,4],[9,16,25,36],[49,64]]  
--map2 fst [(1,2),(3,5),(6,3),(2,6),(2,5)] = [1,3,6,2,2]  

iter :: (a -> a) -> a -> [a]
iter f = unfold (const False) id f

--take 10 $ iter (*2) 1 = [1,2,4,8,16,32,64,128,256,512]  
--take 3 $ iter (++ "haha") "haha" = ["haha","hahahaha","hahahahahaha"]

