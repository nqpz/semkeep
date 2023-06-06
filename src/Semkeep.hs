{-# LANGUAGE LambdaCase #-}
module Semkeep where

newtype Name = Name String
  deriving (Eq, Show)

data Exp = Symbol Name
         | Const Int
         | LetIn Name Exp Exp
         | Mul Exp Exp
  deriving (Eq, Show)

formatExp :: Exp -> String
formatExp = \case
  Symbol (Name s) -> s
  Const k -> show k
  LetIn (Name s) e1 e2 -> "(let " ++ s ++ " = " ++ formatExp e1 ++ "\nin " ++ formatExp e2 ++ ")"
  Mul e1 e2 -> "(" ++ formatExp e1 ++ " * " ++ formatExp e2 ++ ")"

-- data ExpRel = Assoc
--   deriving (Show)

assocMulDrawLeft :: Exp -> Exp
assocMulDrawLeft (Mul e1 (Mul e2 e3)) = Mul (Mul e1 e2) e3
assocMulDrawLeft _ = undefined

assocMulDrawRight :: Exp -> Exp
assocMulDrawRight (Mul (Mul e1 e2) e3) = Mul e1 (Mul e2 e3)
assocMulDrawRight _ = undefined

recurseMulLeft :: (Exp -> Exp) -> Exp -> Exp
recurseMulLeft f e = case e of
  Mul e1 e2 -> Mul (f e1) e2
  _ -> e

recurseMulRight :: (Exp -> Exp) -> Exp -> Exp
recurseMulRight f e = case e of
  Mul e1 e2 -> Mul e1 (f e2)
  _ -> e

subst :: Exp -> Exp -> Exp -> Exp
subst pat repl = substExp
  where substExp e | e == pat = repl
                   | otherwise = case e of
                       LetIn v e1 e2 -> LetIn v (substExp e1) (substExp e2)
                       Mul e1 e2 -> Mul (substExp e1) (substExp e2)
                       _ -> e

recurse :: (a -> a) -> a -> Int -> a
recurse _next cur 0 = cur
recurse next cur n_steps = recurse next (next cur) (n_steps - 1)
