module Task3 where


sentence :: (String, String, Integer) -> String
sentence (name, study, year) = 
    name ++ " is studying at " ++ study ++ " department and started in " ++ show year


information :: [String] -> [String] -> [Integer] -> [String]
information names studies years = map sentence info 
    where
        filteredYears = filter (>= 2022) years 
        info = zip3 names studies filteredYears

-------------------------------------------------------------------------------------------------------------

semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]
semiFermat n m = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n], a^m + b ^m == c^(m-1)]


-------------------------------------------------------------------------------------------------------------


numberToWord :: Integer -> String
