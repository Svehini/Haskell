module BinarySearchTree where

data BinSearchTree a
  = Empty
  | Branch (BinSearchTree a) a (BinSearchTree a)
  deriving (Eq, Show)

toBinarySearchTree :: [a] -> BinSearchTree a
toBinarySearchTree [] = Empty
toBinarySearchTree xs = Branch(toBinarySearchTree left) mid (toBinarySearchTree right)
    where
        (left, mid:right) = splitAt (div (length xs) 2) xs