{-# LANGUAGE LambdaCase #-}
module Lib where

import Control.Exception (assert)

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

data ExpRel = Assoc
  deriving (Show)

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


intLog :: Int -> Int
intLog = (round :: Double -> Int) . logBase 2 . fromIntegral

pow :: Name -> Int -> Exp
pow k n =
  assert (2^intLog n == n) $ -- FIXME
  case n of
  0 -> Const 1
  _ -> let k' = Symbol k
       in foldr Mul k' (replicate (n - 1) k')

pow' :: Name -> Int -> Exp
pow' k n =
  assert (2^intLog n == n) $ -- FIXME
  foldr Mul (Const 1) $ map Symbol $ replicate n k

-- 4: assocMulDrawLeft
-- 8: r (r assocMulDrawLeft)
-- 16: r (r (r (r (r (r assocMulDrawLeft)))))
-- 32: r (r (r (r (r (r (r (r (r (r (r (r (r (r assocMulDrawLeft)))))))))))))

subst :: Exp -> Exp -> Exp -> Exp
subst pat repl = substExp
  where substExp e | e == pat = repl
                   | otherwise = case e of
                       LetIn v e1 e2 -> LetIn v (substExp e1) (substExp e2)
                       Mul e1 e2 -> Mul (substExp e1) (substExp e2)
                       _ -> e



powOpt :: Name -> Int -> Exp -> Exp
powOpt v n e =
  let r f = recurseMulRight f . assocMulDrawLeft
      rep f b = \case
        0 -> b
        k -> f (rep f b (k - 1))
      rep' = rep r assocMulDrawLeft
      nest e' = \case
        4 -> rep' 0 e'
        k -> let k' = k `div` 2
             in nest (rep' (k' - 2) e') k'
      expGrouped = nest e n
      li v_prev i e' = case i of
        1 -> e'
        _ -> let e_new = Mul (Symbol v_prev) (Symbol v_prev)
                 v_new = Name ("v" ++ show i)
             in LetIn v_new e_new $ li v_new (i - 1) $ subst e_new (Symbol v_new) e'
  in li v (intLog n) expGrouped

powWithOpt :: Name -> Int -> Exp
powWithOpt k n = powOpt k n $ pow k n
