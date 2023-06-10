{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Exception (assert)
import qualified Exp as E
import qualified Calc as C

intLog :: Int -> Int
intLog = (round :: Double -> Int) . logBase 2 . fromIntegral

pow :: E.Var -> Int -> E.Exp
pow v n =
  assert (n > 0 && 2^intLog n == n) $ -- FIXME
  let v' = E.Symbol v
  in E.recurse (E.Mul v') id id v' (n - 1)

pow' :: E.Var -> E.Var -> C.Val E.Exp
pow' v n =
  let v' = E.Symbol v
      next = C.Body (C.Apply (C.BinOp C.ExpMul) (C.Tup C.Arg1 (C.Lit v')))
      n_minus_one = C.Apply (C.BinOp C.Sub) (C.Tup (C.Var n) (C.Lit 1))
      fun = C.Recurse next n_minus_one
  in C.Apply fun (C.Lit v')

type Next a = a -> a
type MakeSubstsAcc = (E.Exp -> E.Exp, E.Exp, E.Var, Int)

powOpt :: E.Var -> Int -> E.Exp -> E.Exp
powOpt v n = makeSubsts . group
  where nLog :: Int
        nLog = intLog n

        groupStep :: E.Exp -> Int -> E.Exp
        groupStep e = E.recurse next (const E.assocMulLeft) ($ e) e
          where next :: Next (E.Exp -> E.Exp)
                next f = E.transformMulRight f . E.assocMulLeft

        group :: E.Exp -> E.Exp
        group e = E.recurse next (n,) snd e (nLog - 1)
          where next :: Next (Int, E.Exp)
                next (k, e') =
                  let k' = k `div` 2
                  in (k', groupStep e' (k' - 2))

        makeSubsts :: E.Exp -> E.Exp
        makeSubsts e = E.recurse next fromExp toExp e nLog
          where next :: Next MakeSubstsAcc
                next (f, e_prev, v_prev, i) =
                  let e_new = E.Mul (E.Symbol v_prev) (E.Symbol v_prev)
                      v_new = E.Var ("v" ++ show i)
                  in (f . E.LetIn v_new e_new,
                      E.subst e_new (E.Symbol v_new) e_prev,
                      v_new, i + 1)

                fromExp :: E.Exp -> MakeSubstsAcc
                fromExp e' = (id, e', v, 0)

                toExp :: MakeSubstsAcc -> E.Exp
                toExp (let_ins, res, _, _) = let_ins res

type Next' a = C.Fun a a
-- -- type MakeSubstsAccCore = (C.Fun E.Exp E.Exp, E.Exp)
-- -- type MakeSubstsAcc' = ((MakeSubstsAccCore, E.Var), Int)
-- type MakeSubstsAcc' = (C.Fun E.Exp E.Exp, E.Exp)

powOpt' :: E.Var -> E.Var -> C.Fun E.Exp E.Exp
powOpt' v n = C.Compose group (C.Subst (E.Mul E.Arg1 E.Arg1))
  where nLog :: C.Val Int
        nLog = C.Apply (C.UnOp C.Log2) (C.Var n)

        groupStep :: C.Val Int -> C.Val (C.Fun E.Exp E.Exp)
        groupStep m = C.Apply recurser (C.Lit (C.AssocMul C.LeftOf))
          where recurser :: Next' (C.Fun E.Exp E.Exp)
                recurser = C.Recurse next m

                next :: Next' (C.Fun E.Exp E.Exp)
                next = C.Body (C.Lit (C.Compose (C.AssocMul C.LeftOf) (C.TransformMul C.RightOf C.Arg1)))

        group :: C.Fun E.Exp E.Exp
        group = C.Body $ C.Snd $ C.Apply recurser $ C.Tup (C.Var n) C.Arg1
          where recurser :: Next' (Int, E.Exp)
                recurser = C.Recurse next nLogMinusOne

                next :: Next' (Int, E.Exp)
                next = C.Body
                       $ C.LetIn2 a1 a2
                       $ C.Tup (C.Arg1 :: C.Val Int) (C.Apply' groupStep' (C.Arg2 :: C.Val E.Exp))
                  where groupStep' :: C.Val (C.Fun E.Exp E.Exp)
                        groupStep' = groupStep (C.Apply (C.BinOp C.Sub) (C.Tup C.Arg1 (C.Lit 2)))

                        bodyArg :: C.Val (Int, E.Exp)
                        bodyArg = C.Arg1

                        a1 :: C.Val Int
                        a1 = C.Apply (C.BinOp C.Div) (C.Tup (C.Fst bodyArg) (C.Lit 2))

                        a2 :: C.Val E.Exp
                        a2 = C.Snd bodyArg

                nLogMinusOne :: C.Val Int
                nLogMinusOne = C.Apply (C.BinOp C.Sub) (C.Tup nLog (C.Lit 1))

        -- makeSubsts :: C.Fun E.Exp E.Exp
        -- makeSubsts = C.Body $ toExp $ C.LetIn1 (C.Var v) $ C.Apply recurser $ fromExp C.Arg1
        --   where recurser :: Next' MakeSubstsAcc'
        --         recurser = C.Recurse next nLog

        --         next :: Next' MakeSubstsAcc'
        --         next = C.Body
        --                $ C.LetIn2 (C.Fst msa) (C.Snd msa)
        --                $ C.Subst (E.Mul E.Arg1 E.Arg1)
        --           where msa :: C.Val MakeSubstsAcc'
        --                 msa = C.Arg1

        --         -- next (f, e_prev, v_prev, i) =
        --         --   let e_new = E.Mul (E.Symbol v_prev) (E.Symbol v_prev)
        --         --       v_new = E.Var ("v" ++ show i)
        --         --   in (f . E.LetIn v_new e_new,
        --         --       E.subst e_new (E.Symbol v_new) e_prev,
        --         --       v_new, i + 1)

        --         fromExp :: C.Val E.Exp -> C.Val MakeSubstsAcc'
        --         fromExp e = C.Tup (C.Lit (C.UnOp C.Id)) e

        --         toExp :: C.Val MakeSubstsAcc' -> C.Val E.Exp
        --         toExp msa = C.LetIn2 (C.Fst msa) (C.Snd msa)
        --                     $ C.Apply' (C.Arg1 :: C.Val (C.Fun E.Exp E.Exp)) (C.Arg2 :: C.Val E.Exp)

        --         -- fromExp :: C.Val E.Exp -> C.Val MakeSubstsAcc'
        --         -- fromExp e = C.Tup (C.Tup (C.Tup (C.Lit (C.UnOp C.Id)) e) (C.Var v)) (C.Lit 0)

        --         -- toExp :: C.Val MakeSubstsAcc' -> C.Val E.Exp
        --         -- toExp msa = C.LetIn1 (C.Fst $ C.Fst msa)
        --         --             $ C.LetIn2 (C.Fst li1Arg) (C.Snd li1Arg)
        --         --             $ C.Apply' (C.Arg1 :: C.Val (C.Fun E.Exp E.Exp)) (C.Arg2 :: C.Val E.Exp)
        --         --   where li1Arg :: C.Val MakeSubstsAccCore
        --         --         li1Arg = C.Arg1

powWithOpt :: E.Var -> Int -> E.Exp
powWithOpt v n = powOpt v n $ pow v n

main :: IO ()
main = putStrLn $ E.formatExp $ powWithOpt (E.Var "v") 128
