riffle :: [a] -> [a] -> [a]
riffle xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]
