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
  in recurse (Mul v') id id v' (n - 1)

type Next a = a -> a
type MakeSubstsAcc = (Exp -> Exp, Exp, Var, Int)

powOpt :: Var -> Int -> Exp -> Exp
powOpt v n = makeSubsts . group
  where nLog :: Int
        nLog = intLog n

        groupStep :: Exp -> Int -> Exp
        groupStep e = recurse next (const assocMulLeft) ($ e) e
          where next :: Next (Exp -> Exp)
                next f = transformMulRight f . assocMulLeft

        group :: Exp -> Exp
        group e = recurse next (n,) snd e (nLog - 1)
          where next :: Next (Int, Exp)
                next (k, e') =
                  let k' = k `div` 2
                  in (k', groupStep e' (k' - 2))

        makeSubsts :: Exp -> Exp
        makeSubsts e = recurse next fromExp toExp e nLog
          where next :: Next MakeSubstsAcc
                next (f, e_prev, v_prev, i) =
                  let e_new = Mul (Symbol v_prev) (Symbol v_prev)
                      v_new = Var ("v" ++ show i)
                  in (f . LetIn v_new e_new,
                      subst e_new (Symbol v_new) e_prev,
                      v_new, i + 1)

                fromExp :: Exp -> MakeSubstsAcc
                fromExp e' = (id, e', v, 0)

                toExp :: MakeSubstsAcc -> Exp
                toExp (let_ins, res, _, _) = let_ins res

powWithOpt :: Var -> Int -> Exp
powWithOpt v n = powOpt v n $ pow v n

main :: IO ()
main = putStrLn $ formatExp $ powWithOpt (Var "v") 128
