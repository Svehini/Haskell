module Task4 where


runningSum :: [Integer] -> [Integer]
runningSum (x:[]) = x : []
runningSum (x:y:ys) = x : runningSum ((x+y) : ys)


combinations :: Integer -> [Char] -> [String]
combinations 0 _ = [""]
combinations n chars = [ x : xs | x <- chars, xs <- combinations (n-1) chars]


removeNothing :: [Maybe a] -> [a]
removeNothing [] = []
removeNothing (x:xs) = case x of
    Nothing -> removeNothing xs
    Just x ->  x : removeNothing xs
