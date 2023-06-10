{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Calc ( GeneralUse
            , ConstructionOnly
            , ConstructionOrOptimization
            , OpSide(..)
            , UnOp(..)
            , BinOp(..)
            , Fun(..)
            , Val(..)
            , limit
            ) where

import Unsafe.Coerce
import qualified Exp as E

data OpSide = LeftOf | RightOf
  deriving (Eq, Show)

data UnOp a b where
  Id :: UnOp a a
  Log2 :: UnOp Int Int

deriving instance (Show a, Show b) => Show (UnOp a b)

data GeneralUse
data ConstructionOnly

class ConstructionOrOptimization constraint
instance ConstructionOrOptimization GeneralUse
instance ConstructionOrOptimization ConstructionOnly

data BinOp a constraint where
  Add :: BinOp Int GeneralUse
  Sub :: BinOp Int GeneralUse
  Mul :: BinOp Int GeneralUse
  Div :: BinOp Int GeneralUse
  ExpMul :: BinOp E.Exp ConstructionOnly

deriving instance Show a => Show (BinOp a constraint)

data Fun a b constraint where
  AssocMul :: OpSide -> Fun E.Exp E.Exp GeneralUse
  TransformMul :: OpSide -> Val (Fun E.Exp E.Exp constraint) constraint -> Fun E.Exp E.Exp constraint
  Subst :: E.Exp -> Val Int constraint -> Fun E.Exp E.Exp constraint
  Recurse :: ConstructionOrOptimization intConstraint => Fun a a constraint -> Val Int intConstraint -> Fun a a constraint
  Compose :: (Show a, Show b) => Fun a b constraint -> Fun b c constraint -> Fun a c constraint
  UnOp :: UnOp a b -> Fun a b GeneralUse
  BinOp :: BinOp a constraint -> Fun (a, a) a constraint
  Body :: Val b constraint -> Fun a b constraint

deriving instance (Show a, Show b) => Show (Fun a b constraint)

data Val a constraint where
  Lit :: ConstructionOrOptimization constraint => a -> Val a constraint
  Tup :: (Show a, Show b) => Val a constraint -> Val b constraint -> Val (a, b) constraint
  Fst :: (Show a, Show b) => Val (a, b) constraint -> Val a constraint
  Snd :: (Show a, Show b) => Val (a, b) constraint -> Val b constraint
  Apply :: (Show a, Show b) => Fun a b constraint -> Val a constraint -> Val b constraint
  Apply' :: (Show a, Show b) => Val (Fun a b constraint) constraint -> Val a constraint -> Val b constraint
  ArgN :: ConstructionOrOptimization constraint => Val Int constraint
  Arg1 :: ConstructionOrOptimization constraint => Val a constraint
  Arg2 :: ConstructionOrOptimization constraint => Val a constraint
  LetIn1 :: (Show u, Show b) => Val u constraint -> Val b constraint -> Val b constraint
  LetIn2 :: (Show u, Show v, Show b) => Val u constraint -> Val v constraint -> Val b constraint -> Val b constraint

deriving instance Show a => Show (Val a constraint)

limit :: Fun a b GeneralUse -> Fun a b ConstructionOnly
limit = unsafeCoerce -- fixme

-- unLit :: Val a -> a
-- unLit (Lit a) = a

-- unValFun :: Val (Fun a b) -> Fun a b
-- unValFun (Lit fun) = fun
-- -- unValFun (Fst tup) =
