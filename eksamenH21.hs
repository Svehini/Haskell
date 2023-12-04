

takeW :: Int -> String -> String
takeW n str = strAdder $ take n (words str)


strAdder :: [String] -> String
strAdder [] = ""
strAdder (x:[]) = x 
strAdder (x:xs) = x ++ " " ++ strAdder xs


erSubliste :: (Eq a) => [a] -> [a] -> Bool
erSubliste [] _ = False
erSubliste _ [] = False
erSubliste (x:[]) (y:ys) = if x == y 
    then True
    else erSubliste [x] ys

erSubliste (x:xs) (y:ys) = if x == y 
    then erSubliste xs ys
    else erSubliste (x:xs) ys


erSubstreng :: (Eq a) => [a] -> [a] -> Bool
erSubstreng [] _ = False
erSubstreng _ [] = False
erSubstreng (x:xs) (y:ys) = if (x == y)
    then if (checkRest xs ys)
        then True
        else erSubstreng xs ys
    else erSubstreng (x:xs) ys


checkRest :: (Eq a) => [a] -> [a] -> Bool
checkRest [] _ = False
checkRest _ [] = False
checkRest (x:[]) (y:ys) = if x == y
    then True
    else False

checkRest (x:xs) (y:ys) = if x == y
    then checkRest xs ys
    else False


finnFiksPunkt :: (Eq a) => (a -> a) -> a -> Int -> Maybe Int -- Does not work for Maybe a
finnFiksPunkt f x n = equator f x n 0


equator :: (Eq a) => (a -> a) -> a -> Int -> Int -> Maybe Int -- Does not work for Maybe a
equator f x n a = if a == n
    then Nothing
    else if f x == x 
        then Just (a+1)
        else equator f (f x) (n-1) (a+1)


serie :: (Int -> b) -> Int -> [b]
serie f i = [f i] ++ serie f (i+1)


sq :: Int -> [Int]
sq n = take n (serie (^2) 0)


streng :: Int -> String 
streng 0 = ""
streng n = 
    if n == 1
        then ("f("++show n++")")
    else streng(n-1) ++ " ," ++ ("f("++show n++")")


h :: (Int -> Int) -> Int -> [Int]
h f i = hHelper f i 0

hHelper :: (Int -> Int) -> Int -> Int -> [Int]
hHelper f i acc = [sum $ take (acc+1) (serie f i)] ++ hHelper f i (acc+1)


-- hva xs = foldr(++) [] (map sing xs), where sing x = [x]
-- hva blir resutltatet av hva [1,2,3]

-- Denne vil bruke sing x til å lage en liste av hvert element i xs, og foldr vil legge disse listene sammen slik:
-- [x] ++ ([x] ++ ([x]) ++ foldr []), som igjen blir [1,2,3]

-- map ?1 . filter ?2 = filter (>0) . map (+1) .
-- ?1 = (+1), ?2 = (>-1), slik at
-- map (+1) . filter ?2 = filter (>-1) = filter (>0) . map (+1)
