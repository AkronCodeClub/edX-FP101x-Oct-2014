
mapM6 f [] = return []
mapM6 f (a : as)
  = do b <- f a
       bs <- mapM6 f as
       return (b : bs)

mapM7 f [] = return []
mapM7 f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM7 f as
           return (b : bs)
           
mapM8 f [] = return []
mapM8 f (a : as)
  = f a >>=
      \ b ->
        do bs <- mapM8 f as
           return (bs ++ [b])
           
filterM2 _ [] = return []
filterM2 p (x : xs)
  = do flag <- p x
       ys <- filterM2 p xs
       if flag then return (x : ys) else return ys

liftM1' f m
  = do x <- m
       return (f x)

liftM2' f m = m >>= \ a -> f a

liftM3' f m = m >>= \ a -> return (f a) 

liftM4' f m = return (f m)

liftM5' f m = m >>= \a -> m >>= \b -> return (f a)

liftM6' f m = m >>= \a -> m >>= \b -> return (f b)

liftM7' f m = mapM f [m]

liftM8' f m = m >> \ a -> return (f a)
