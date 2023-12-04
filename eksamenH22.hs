import Data.Char (toLower, toUpper)
import Data.Either (either)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (intersperse)
import Control.Applicative (liftA2)

main :: IO ()
main = do 
    putStrLn "Hva heter du til fornavn? "
    (n:ns) <- fmap (reverse . map toLower) getLine
    putStrLn $ "Hei! " ++ [toUpper n] ++ ns


sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum $ [ x^2 | x <- xs]

sumOfSquares2 :: [Integer] -> Integer
sumOfSquares2 xs = sum $ map (^2) xs

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x] ++ [x] ++ duplicate xs
-- duplicate (x:xs) = x : x : duplicate xs fungerer også

-- f = map snd . filter (odd . fst) . zip [0..]
-- f "abcdefg" gir ['b', 'd', 'f'] fordi:
-- map snd . filter (odd . fst) . zip [0..] "abcdefg"
-- map snd . filter (odd . fst) [(0,a), (1,b), (2,c), (3,d), (4,e), (5,f), (6,g)]
-- map snd . filter (odd . fst) [(0,a), (1,b), (2,c), (3,d), (4,e), (5,f), (6,g)] tar bare odds som er første i alle tuplene
-- map snd [(1,b), (3,d), (5,f)] returnerer bare snd i hver tupel
-- => ['b','d','f'] => "bdf"

foldrDuplicate :: [Integer] -> Integer
foldrDuplicate xs = foldr (*) 1 xs 

hasLength :: Int -> [a] -> Bool
hasLength n list  = length list == n 

hasLength2 :: Int -> [a] -> Bool
hasLength2 n xs = accLength n xs 0

accLength :: Int -> [a] -> Int -> Bool
accLength n [] a = if n /= a 
    then False
    else True

accLength n (x:xs) a = if a < n 
    then accLength n xs (a+1)
    else False
