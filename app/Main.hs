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

powWithOpt :: E.Var -> Int -> E.Exp
powWithOpt v n = powOpt v n $ pow v n


pow' :: C.Val E.Exp C.ConstructionOnly C.WithArg1NotUsed C.WithArg2NotUsed
pow' = C.ApplyCalculated (C.Recurse next nMinusOne) C.ArgV
  where next :: C.Val E.Exp C.ConstructionOnly (C.WithArg1Used E.Exp) C.WithArg2NotUsed
        next = C.BinOp C.ExpMul C.Arg1 C.ArgV

        nMinusOne :: C.Val Int C.GeneralUse C.WithArg1NotUsed C.WithArg2NotUsed
        nMinusOne = C.BinOp C.Sub C.ArgN (C.Lit 1)

powOpt' :: C.Val E.Exp C.GeneralUse (C.WithArg1Used E.Exp) C.WithArg2NotUsed
powOpt' = C.LetIn1 group $ C.Subst (E.Mul E.ArgV E.ArgV) nLog C.Arg1
  where nLog = C.UnOp C.Log2 C.ArgN -- FIXME: Don't calculate this twice (see nLogMinusOne)

--        groupStep :: (C.Arg1 arg1, C.Arg2 arg2)
        groupStep :: C.Val Int C.GeneralUse (C.WithArg1Used Int) C.WithArg2NotUsed -- arg1 arg2
                  -> C.Val (C.Fun E.Exp E.Exp) C.GeneralUse (C.WithArg1Used Int) C.WithArg2NotUsed -- arg1 arg2
        groupStep m = C.ApplyCalculated (C.Recurse (C.calculated next) m) (C.Lit $ C.Fun $ C.AssocMul C.LeftOf C.Arg1)
          where next :: C.Val (C.Fun E.Exp E.Exp) C.GeneralUse C.WithArg1NotUsed C.WithArg2NotUsed
                next = C.Compose (C.Lit (C.Fun (C.AssocMul C.LeftOf C.Arg1))) (C.Lit (C.Fun (C.TransformMul C.RightOf C.Arg1)))

        group :: C.Val E.Exp C.GeneralUse (C.WithArg1Used E.Exp) C.WithArg2NotUsed
        group = C.Snd $ C.ApplyCalculated (C.Recurse next nLogMinusOne) calcFunArg
          where next :: C.Val (Int, E.Exp) C.GeneralUse (C.WithArg1Used (Int, E.Exp)) C.WithArg2NotUsed
                next = C.LetIn2 letIn2A letIn2B $ letInRes

                calcFunArg :: C.Val (Int, E.Exp) C.GeneralUse (C.WithArg1Used E.Exp) C.WithArg2NotUsed
                calcFunArg = C.Tup C.ArgN C.Arg1

                letIn2A :: C.Val Int C.GeneralUse (C.WithArg1Used (Int, E.Exp)) C.WithArg2NotUsed
                letIn2A = C.BinOp C.Div divArg1 (C.Lit 2)

                divArg1 :: C.Val Int C.GeneralUse (C.WithArg1Used (Int, E.Exp)) C.WithArg2NotUsed
                divArg1 = C.Fst C.Arg1

                letIn2B :: C.Val E.Exp C.GeneralUse (C.WithArg1Used (Int, E.Exp)) C.WithArg2NotUsed
                letIn2B = C.Snd C.Arg1

                letInRes :: C.Val (Int, E.Exp) C.GeneralUse (C.WithArg1Used Int) (C.WithArg2Used E.Exp)
                letInRes = C.Tup C.Arg1 groupStepApplied

                groupStepArg :: C.Val Int C.GeneralUse (C.WithArg1Used Int) C.WithArg2NotUsed
                groupStepArg = C.BinOp C.Sub C.Arg1 (C.Lit 2)

                groupStepApplied :: C.Val E.Exp C.GeneralUse (C.WithArg1Used Int) (C.WithArg2Used E.Exp)
                groupStepApplied = C.ApplyFun (groupStep groupStepArg) C.Arg2

                nLogMinusOne :: C.Val Int C.GeneralUse C.WithArg1NotUsed C.WithArg2NotUsed
                nLogMinusOne = C.BinOp C.Sub nLog (C.Lit 1)

powWithOpt' :: C.Val E.Exp C.ConstructionOnly C.WithArg1NotUsed C.WithArg2NotUsed
powWithOpt' = C.Apply (C.limit powOpt') pow'

mainExp :: IO ()
mainExp = putStrLn $ E.formatExp $ powWithOpt (E.Var "v") 128

mainCalc :: IO ()
mainCalc = putStrLn $ show $ powWithOpt'

main :: IO ()
main = do
  mainExp
  putStrLn "----------"
  mainCalc
