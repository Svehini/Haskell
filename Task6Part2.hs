module Task6Part2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)


eval :: (Ord variable) => Expr variable -> Map variable Bool -> Maybe Bool
eval (Var var) varMap = Map.lookup var varMap
eval (Lit b) _ = Just b 
eval (And var1 var2) varMap = do
    val1 <- eval var1 varMap
    val2 <- eval var2 varMap
    return (val1 && val2)
eval (Or var1 var2) varMap = do
    val1 <- eval var1 varMap
    val2 <- eval var2 varMap
    return (val1 || val2)