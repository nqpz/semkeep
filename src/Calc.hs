{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module Calc ( GeneralUse
            , ConstructionOnly
            , ConstructionOrOptimization
            , WithArg1Used
            , WithArg1NotUsed
            , WithArg2Used
            , WithArg2NotUsed
            , OpSide(..)
            , UnOp(..)
            , BinOp(..)
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

data WithArg1Used a
data WithArg1NotUsed
data WithArg2Used a
data WithArg2NotUsed

class MergeUsedArg1 a b m
class MergeUsedArg2 a b m

instance MergeUsedArg1 WithArg1NotUsed WithArg1NotUsed WithArg1NotUsed
instance MergeUsedArg1 WithArg1NotUsed (WithArg1Used a) (WithArg1Used a)
instance MergeUsedArg1 (WithArg1Used a) WithArg1NotUsed (WithArg1Used a)

instance MergeUsedArg2 WithArg2NotUsed WithArg2NotUsed WithArg2NotUsed
instance MergeUsedArg2 WithArg2NotUsed (WithArg2Used a) (WithArg2Used a)
instance MergeUsedArg2 (WithArg2Used a) WithArg2NotUsed (WithArg2Used a)

data Val a constraint arg1 arg2 where
  Lit :: ConstructionOrOptimization constraint
      => a
      -> Val a constraint WithArg1NotUsed WithArg2NotUsed

  Tup :: (Show a, Show b, MergeUsedArg1 arg1A arg1B arg1Merged, MergeUsedArg2 arg2A arg2B arg2Merged)
      => Val a constraint arg1A arg2A
      -> Val b constraint arg1B arg2B
      -> Val (a, b) constraint arg1Merged arg2Merged

  Fst :: (Show a, Show b)
      => Val (a, b) constraint arg1 arg2
      -> Val a constraint arg1 arg2

  Snd :: (Show a, Show b)
      => Val (a, b) constraint arg1 arg2
      -> Val b constraint arg1 arg2

  ArgN :: ConstructionOrOptimization constraint
       => Val Int constraint WithArg1NotUsed WithArg2NotUsed

  Arg1 :: ConstructionOrOptimization constraint
       => Val a constraint (WithArg1Used a) WithArg2NotUsed

  Arg2 :: ConstructionOrOptimization constraint
       => Val a constraint WithArg1NotUsed (WithArg2Used a)

  LetIn1 :: (Show u, Show a)
         => Val u constraint arg1Root arg2Root
         -> Val a constraint (WithArg1Used u) WithArg2NotUsed
         -> Val a constraint arg1Root arg2Root

  LetIn2 :: (Show u, Show v, Show a, MergeUsedArg1 arg1RootA arg1RootB arg1RootMerged, MergeUsedArg2 arg2RootA arg2RootB arg2RootMerged)
         => Val u constraint arg1RootA arg2RootA
         -> Val v constraint arg1RootB arg2RootB
         -> Val a constraint (WithArg1Used u) (WithArg2Used v)
         -> Val a constraint arg1RootMerged arg2RootMerged

  AssocMul :: OpSide
           -> Val E.Exp GeneralUse arg1 arg2
           -> Val E.Exp GeneralUse arg1 arg2

  TransformMul :: OpSide
               -> Val E.Exp GeneralUse (WithArg1Used E.Exp) WithArg2NotUsed
               -> Val E.Exp GeneralUse WithArg1NotUsed WithArg2NotUsed

  Subst :: (MergeUsedArg1 arg1A arg1B arg1Merged, MergeUsedArg2 arg2A arg2B arg2Merged)
        => E.Exp
        -> Val Int GeneralUse arg1A arg2A
        -> Val E.Exp GeneralUse arg1B arg2B
        -> Val E.Exp GeneralUse arg1Merged arg2Merged

  Recurse :: (ConstructionOrOptimization constraint, ConstructionOrOptimization intConstraint)
          => Val a constraint (WithArg1Used a) WithArg2NotUsed
          -> Val Int intConstraint arg1 arg2
          -> Val a constraint arg1 arg2

  UnOp :: (Show a, Show b, ConstructionOrOptimization constraint)
       => UnOp a b
       -> Val a constraint WithArg1NotUsed WithArg2NotUsed
       -> Val b constraint WithArg1NotUsed WithArg2NotUsed

  BinOp :: (Show a, ConstructionOrOptimization constraint)
        => BinOp a constraint
        -> Val (a, a) constraint WithArg1NotUsed WithArg2NotUsed
        -> Val a constraint WithArg1NotUsed WithArg2NotUsed

deriving instance Show a => Show (Val a constraint arg1 arg2)

limit :: Val a GeneralUse arg1 arg2 -> Val a ConstructionOnly arg1 arg2
limit = unsafeCoerce -- fixme
