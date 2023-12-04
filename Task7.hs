module Task7 where


applyFunctions :: [a -> b] -> [b -> c] -> [a] -> [c]
applyFunctions _ _ [] = []
applyFunctions _ [] _ = []
applyFunctions [] _ _ = []
applyFunctions (x:xs) (y:ys) (z:zs) = (y $ x z) : applyFunctions xs ys zs 


fromLeftAndRight :: (Either a b -> c) -> (a -> c, b -> c)
fromLeftAndRight f = (leftFunc, rightFunc)
    where
        leftFunc a = f (Left a)
        rightFunc b = f (Right b)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left a ) = f a 
either' f g (Right b) = g b

toFstAndSnd :: (a -> (b, c)) -> (a -> b, a -> c)
toFstAndSnd f = (fstFunc, sndFunc)
    where
        fstFunc a = fst (f a) -- fst is a function that takes the first element of a tuple
        sndFunc a = snd (f a) -- snd is a function that takes the seccond element of a tuple


pair :: (a -> b) -> (a -> c) -> a -> (b, c)
pair f g x = (f x, g x)


isFiveMultiples :: [Integer] ->  Bool
isFiveMultiples = all $ (==0) . (`mod` 5)

