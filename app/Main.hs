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


type Next' a constraint = C.Fun a a constraint

argV :: C.ConstructionOrOptimization constraint => C.Val E.Exp constraint
argV = C.Lit E.ArgV

pow' :: C.Val E.Exp C.ConstructionOnly
pow' = C.Apply recurser argV
  where recurser :: Next' E.Exp C.ConstructionOnly
        recurser = C.Recurse next nMinusOne

        next :: Next' E.Exp C.ConstructionOnly
        next = C.Body (C.Apply (C.BinOp C.ExpMul) (C.Tup C.Arg1 argV))

        nMinusOne :: C.Val Int C.GeneralUse
        nMinusOne = C.Apply (C.BinOp C.Sub) (C.Tup C.ArgN (C.Lit 1))

powOpt' :: C.Fun E.Exp E.Exp C.GeneralUse
powOpt' = C.Compose group (C.Subst (E.Mul E.ArgV E.ArgV) nLog)
  where nLog :: C.Val Int C.GeneralUse
        nLog = C.Apply (C.UnOp C.Log2) C.ArgN -- this is calculated twice, see nLogMinusOne, maybe fix this

        groupStep :: C.Val Int C.GeneralUse -> C.Val (C.Fun E.Exp E.Exp C.GeneralUse) C.GeneralUse
        groupStep m = C.Apply recurser (C.Lit (C.AssocMul C.LeftOf))
          where recurser :: Next' (C.Fun E.Exp E.Exp C.GeneralUse) C.GeneralUse
                recurser = C.Recurse next m

                next :: Next' (C.Fun E.Exp E.Exp C.GeneralUse) C.GeneralUse
                next = C.Body (C.Lit (C.Compose (C.AssocMul C.LeftOf) (C.TransformMul C.RightOf C.Arg1)))

        group :: C.Fun E.Exp E.Exp C.GeneralUse
        group = C.Body $ C.Snd $ C.Apply recurser $ C.Tup C.ArgN C.Arg1
          where recurser :: Next' (Int, E.Exp) C.GeneralUse
                recurser = C.Recurse next nLogMinusOne

                next :: Next' (Int, E.Exp) C.GeneralUse
                next = C.Body
                       $ C.LetIn2 a1 a2
                       $ C.Tup (C.Arg1 :: C.Val Int C.GeneralUse) (C.Apply' groupStep' (C.Arg2 :: C.Val E.Exp C.GeneralUse))
                  where groupStep' :: C.Val (C.Fun E.Exp E.Exp C.GeneralUse) C.GeneralUse
                        groupStep' = groupStep (C.Apply (C.BinOp C.Sub) (C.Tup C.Arg1 (C.Lit 2)))

                        bodyArg :: C.Val (Int, E.Exp) C.GeneralUse
                        bodyArg = C.Arg1

                        a1 :: C.Val Int C.GeneralUse
                        a1 = C.Apply (C.BinOp C.Div) (C.Tup (C.Fst bodyArg) (C.Lit 2))

                        a2 :: C.Val E.Exp C.GeneralUse
                        a2 = C.Snd bodyArg

                nLogMinusOne :: C.Val Int C.GeneralUse
                nLogMinusOne = C.Apply (C.BinOp C.Sub) (C.Tup nLog (C.Lit 1))

powWithOpt' :: C.Val E.Exp C.ConstructionOnly
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
