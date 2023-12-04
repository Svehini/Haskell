


data BinSearchTree a
  = Leaf a
  | LeftRightChildBranch (BinSearchTree a) a (BinSearchTree a)
  | LeftChildBranch (BinSearchTree a) a
  | RightChildBranch a (BinSearchTree a)
  deriving (Eq, Show)

instance Foldable BinSearchTree where
    foldr f z (Leaf a) = f a z
    foldr f z (LeftRightChildBranch leftTree a rightTree) = foldr f (f a (foldr f z rightTree)) leftTree
    foldr f z (LeftChildBranch leftTree a) = foldr f (f a z) leftTree
    foldr f z (RightChildBranch a rightTree) = f a (foldr f z rightTree)

toList :: BinSearchTree a -> [a]
toList = foldr (:) []

data RoseTree a = Branch a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
    fmap f (Branch a t) = Branch (f a) (map (fmap f) t)

productNodes :: (Num a) => RoseTree [a] -> RoseTree a
productNodes tree = fmap product tree
    where
        product [] = 1
        product xs = foldr (*) 1 xs


class IntegerGraph g where
  emptyGraph :: g
  insertNode :: Integer -> g -> g
  insertEdge :: Integer -> Integer -> g -> g
  nodeInGraph :: Integer -> g -> Bool
  edgeInGraph :: Integer -> Integer -> g -> Bool


data MyGraph = MyGraph [(Integer, [Integer])]

instance Show MyGraph where
    show (MyGraph nodes) = show nodes

instance IntegerGraph MyGraph where 
    emptyGraph = emptyGraph []
    insertNode n (MyGraph nodes) = 