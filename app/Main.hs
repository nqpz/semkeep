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

-- pow' :: Name -> Int -> Exp
-- pow' k n =
--   assert (2^intLog n == n) $ -- FIXME
--   foldr Mul (Const 1) $ map Symbol $ replicate n k

powOpt :: Name -> Int -> Exp -> Exp
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
                groupStep = recurse (\f -> recurseMulRight f . assocMulDrawLeft) assocMulDrawLeft

        makeSubsts :: Exp -> Exp
        makeSubsts e =
          let (let_ins, res, _, _) = recurse next (id, e, v, 0) nLog
          in let_ins res
          where next :: (Exp -> Exp, Exp, Name, Int) -> (Exp -> Exp, Exp, Name, Int)
                next (f, e_prev, v_prev, i) =
                  let e_new = Mul (Symbol v_prev) (Symbol v_prev)
                      v_new = Name ("v" ++ show i)
                  in (f . LetIn v_new e_new,
                      subst e_new (Symbol v_new) e_prev,
                      v_new, i + 1)

powWithOpt :: Name -> Int -> Exp
powWithOpt k n = powOpt k n $ pow k n

main :: IO ()
main = putStrLn $ formatExp $ powWithOpt (Name "a") 128
