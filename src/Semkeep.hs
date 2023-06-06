{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Semkeep where

newtype Var = Var String
  deriving (Eq, Show)

data Exp = Symbol Var
         | Const Int
         | LetIn Var Exp Exp
         | Mul Exp Exp
  deriving (Eq, Show)

formatExp :: Exp -> String
formatExp = \case
  Symbol (Var s) -> s
  Const k -> show k
  LetIn (Var s) e1 e2 -> "(let " ++ s ++ " = " ++ formatExp e1 ++ "\nin " ++ formatExp e2 ++ ")"
  Mul e1 e2 -> "(" ++ formatExp e1 ++ " * " ++ formatExp e2 ++ ")"

assocMulLeft :: Exp -> Exp
assocMulLeft (Mul e1 (Mul e2 e3)) = Mul (Mul e1 e2) e3
assocMulLeft _ = undefined

assocMulRight :: Exp -> Exp
assocMulRight (Mul (Mul e1 e2) e3) = Mul e1 (Mul e2 e3)
assocMulRight _ = undefined

transformMulLeft :: (Exp -> Exp) -> Exp -> Exp
transformMulLeft f e = case e of
  Mul e1 e2 -> Mul (f e1) e2
  _ -> e

transformMulRight :: (Exp -> Exp) -> Exp -> Exp
transformMulRight f e = case e of
  Mul e1 e2 -> Mul e1 (f e2)
  _ -> e

subst :: Exp -> Exp -> Exp -> Exp
subst pat repl = substExp
  where substExp e | e == pat = repl
                   | otherwise = case e of
                       LetIn v e1 e2 -> LetIn v (substExp e1) (substExp e2)
                       Mul e1 e2 -> Mul (substExp e1) (substExp e2)
                       _ -> e

recurse :: forall a. (a -> a) -> (Exp -> a) -> (a -> Exp) -> Exp -> Int -> Exp
recurse next from_exp to_exp initial n = to_exp (recurse' (from_exp initial) n)
  where recurse' :: a -> Int -> a
        recurse' cur 0 = cur
        recurse' cur i = recurse' (next cur) (i - 1)
