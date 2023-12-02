module Task2 where
 
f1 :: String -> Char -> Bool
f1 str char = elem char str


f2 :: [Integer] -> [t] -> [(Integer, t)]
f2 xs str = zip xs revstr
    where
        revstr = reverse str


divider :: String -> (String, String)
divider str = if odd $ length str
    then (take mid str, drop (mid+1) str)
    else (take mid str, drop mid str)
    where
        mid = div (length str) 2


semiRepetitive :: String -> Maybe String
semiRepetitive str = if firstHalf == lastHalf
    then Just firstHalf
    else Nothing
    where 
        (firstHalf, lastHalf) = divider str


decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive str = if firstHalf == lastHalf
    then if odd $ length str
        then Just (firstHalf, Just middle)
        else Just (firstHalf, Nothing)
    else Nothing
    where 
        (firstHalf, lastHalf) = divider str
        mid = div (length str) 2
        middle = head $ drop mid str


toChar :: Maybe Char -> String
toChar char = case char of
    Nothing -> ""
    Just char -> [char]


createSemiRepetitive :: String -> Maybe Char -> String
createSemiRepetitive s Nothing = s ++ s
createSemiRepetitive s char = s ++ toChar char ++ s