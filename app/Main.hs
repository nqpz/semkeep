{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Exception (assert)
import Semkeep

intLog :: Int -> Int
intLog = (round :: Double -> Int) . logBase 2 . fromIntegral

pow :: Var -> Int -> Exp
pow v n =
  assert (n > 0 && 2^intLog n == n) $ -- FIXME
  let v' = Symbol v
  in recurse (Mul v') v' (n - 1)

powOpt :: Var -> Int -> Exp -> Exp
powOpt v n = makeSubsts . group
  where nLog :: Int
        nLog = intLog n

        group :: Exp -> Exp
        group e = snd $ recurse next (n, e) (nLog - 1)
          where next :: (Int, Exp) -> (Int, Exp)
                next (k, e') =
                  let k' = k `div` 2
                  in (k', groupStep (k' - 2) e')

                groupStep :: Int -> Exp -> Exp
                groupStep = recurse (\f -> transformMulRight f . assocMulLeft) assocMulLeft

        makeSubsts :: Exp -> Exp
        makeSubsts e =
          let (let_ins, res, _, _) = recurse next (id, e, v, 0) nLog
          in let_ins res
          where next :: (Exp -> Exp, Exp, Var, Int) -> (Exp -> Exp, Exp, Var, Int)
                next (f, e_prev, v_prev, i) =
                  let e_new = Mul (Symbol v_prev) (Symbol v_prev)
                      v_new = Var ("v" ++ show i)
                  in (f . LetIn v_new e_new,
                      subst e_new (Symbol v_new) e_prev,
                      v_new, i + 1)

powWithOpt :: Var -> Int -> Exp
powWithOpt v n = powOpt v n $ pow v n

main :: IO ()
main = putStrLn $ formatExp $ powWithOpt (Var "v") 128
