{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Calc ( OpSide(..)
            , UnOp(..)
            , BinOp(..)
            , Fun(..)
            , Val(..)
            ) where

import qualified Exp as E

data OpSide = LeftOf | RightOf
  deriving (Eq, Show)

data UnOp a b where
  Id :: UnOp a a
  Log2 :: UnOp Int Int

deriving instance (Show a, Show b) => Show (UnOp a b)

data BinOp a where
  Add :: BinOp Int
  Sub :: BinOp Int
  Mul :: BinOp Int
  Div :: BinOp Int
  ExpMul :: BinOp E.Exp -- only for construction, not optimization

deriving instance Show a => Show (BinOp a)

data Fun a b where
  AssocMul :: OpSide -> Fun E.Exp E.Exp
  TransformMul :: OpSide -> Val (Fun E.Exp E.Exp) -> Fun E.Exp E.Exp
  Subst :: E.Exp -> Fun E.Exp E.Exp
  Recurse :: Fun a a -> Val Int -> Fun a a
  Compose :: (Show a, Show b) => Fun a b -> Fun b c -> Fun a c
  UnOp :: UnOp a b -> Fun a b
  BinOp :: BinOp a -> Fun (a, a) a
  Body :: Val b -> Fun a b

deriving instance (Show a, Show b) => Show (Fun a b)

data Val a where
  Lit :: a -> Val a
  Tup :: (Show a, Show b) => Val a -> Val b -> Val (a, b)
  Fst :: (Show a, Show b) => Val (a, b) -> Val a
  Snd :: (Show a, Show b) => Val (a, b) -> Val b
  Var :: E.Var -> Val a
  Apply :: (Show a, Show b) => Fun a b -> Val a -> Val b
  Apply' :: (Show a, Show b) => Val (Fun a b) -> Val a -> Val b
  Arg1 :: Val a
  Arg2 :: Val a
  LetIn1 :: (Show u, Show b) => Val u -> Val b -> Val b
  LetIn2 :: (Show u, Show v, Show b) => Val u -> Val v -> Val b -> Val b

deriving instance Show a => Show (Val a)

-- unLit :: Val a -> a
-- unLit (Lit a) = a

-- unValFun :: Val (Fun a b) -> Fun a b
-- unValFun (Lit fun) = fun
-- -- unValFun (Fst tup) =
