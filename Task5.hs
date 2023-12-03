module Task5 where

data SemiRepetitive = SR String (Maybe Char)
    deriving Show

semiRepetitive :: String -> Maybe SemiRepetitive
semiRepetitive str = if firstHalf == lastHalf
    then Just (SR str middle)
    else Nothing
    where 
        mid = div (length str) 2
        firstHalf = take mid str
        lastHalf = reverse $ take mid (reverse str)
        middle = if odd $ length str
            then Just (head $ drop mid str)
            else Nothing


toChar :: Maybe Char -> String
toChar char = case char of
    Nothing -> ""
    Just char -> [char]

toString :: SemiRepetitive -> String
toString (SR str char) = case char of 
    Nothing -> str ++ str
    Just char -> str ++ (toChar (Just char)) ++ str

