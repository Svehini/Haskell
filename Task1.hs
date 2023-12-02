module Task1 where 

f :: Integer -> Integer
f n = n * div (n+1) 2


triangleNumber :: Integer -> Integer
triangleNumber n = sum[1..n]

palindrome :: [a]->[a]
palindrome xs = xs ++ reverse xs