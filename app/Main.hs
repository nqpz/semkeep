{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Exception (assert)
import Semkeep

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

main :: IO ()
main = pure ()
