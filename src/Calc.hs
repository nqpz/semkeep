{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Calc ( GeneralUse
            , ConstructionOnly
            , ConstructionOrOptimization
            , Arg1
            , Arg2
            -- , MergeUsedArg1
            -- , MergeUsedArg2
            , WithArg1Used
            , WithArg1NotUsed
            , WithArg2Used
            , WithArg2NotUsed
            , OpSide(..)
            , UnOp(..)
            , BinOp(..)
            , Fun(..)
            , Val(..)
            , formatVal
            , limit
            , calculated
            ) where

import Data.List (intercalate)
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

class Arg1 a
instance Arg1 (WithArg1Used a)
instance Arg1 WithArg1NotUsed

class Arg2 a
instance Arg2 (WithArg2Used a)
instance Arg2 WithArg2NotUsed

class MergeUsedArg1 a b m | a b -> m
class MergeUsedArg2 a b m | a b -> m

instance MergeUsedArg1 WithArg1NotUsed WithArg1NotUsed WithArg1NotUsed
instance MergeUsedArg1 WithArg1NotUsed (WithArg1Used a) (WithArg1Used a)
instance MergeUsedArg1 (WithArg1Used a) WithArg1NotUsed (WithArg1Used a)
instance MergeUsedArg1 (WithArg1Used a) (WithArg1Used a) (WithArg1Used a)

instance MergeUsedArg2 WithArg2NotUsed WithArg2NotUsed WithArg2NotUsed
instance MergeUsedArg2 WithArg2NotUsed (WithArg2Used a) (WithArg2Used a)
instance MergeUsedArg2 (WithArg2Used a) WithArg2NotUsed (WithArg2Used a)
instance MergeUsedArg2 (WithArg2Used a) (WithArg2Used a) (WithArg2Used a)

-- type family Merging1 a b where
--   Merging1 WithArg1NotUsed WithArg1NotUsed = WithArg1NotUsed
--   Merging1 WithArg1NotUsed (WithArg1Used a) = WithArg1Used a
--   Merging1 (WithArg1Used a) WithArg1NotUsed = WithArg1Used a
--   Merging1 (WithArg1Used a) (WithArg1Used a) = WithArg1Used a

data Fun a b = Fun (Val b GeneralUse (WithArg1Used a) WithArg2NotUsed)
  deriving (Show)

data Val a constraint arg1 arg2 where
  Lit ::
    (Show a, ConstructionOrOptimization constraint)
    => a
    -> Val a constraint WithArg1NotUsed WithArg2NotUsed

  LitFun ::
    (Show a, Show b, ConstructionOrOptimization constraint)
    => Fun a b
    -> Val (Fun a b) constraint WithArg1NotUsed WithArg2NotUsed

  Tup ::
    (Show a, Show b,
     Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => Val a constraint arg1A arg2A
    -> Val b constraint arg1B arg2B
    -> Val (a, b) constraint arg1Merged arg2Merged

  Fst ::
    (Show a, Show b, Arg1 arg1, Arg2 arg2)
    => Val (a, b) constraint arg1 arg2
    -> Val a constraint arg1 arg2

  Snd ::
    (Show a, Show b, Arg1 arg1, Arg2 arg2)
    => Val (a, b) constraint arg1 arg2
    -> Val b constraint arg1 arg2

  ArgN ::
    ConstructionOrOptimization constraint
    => Val Int constraint WithArg1NotUsed WithArg2NotUsed

  ArgV ::
    ConstructionOrOptimization constraint
    => Val E.Exp constraint WithArg1NotUsed WithArg2NotUsed

  Arg1 ::
    ConstructionOrOptimization constraint
    => Val a constraint (WithArg1Used a) WithArg2NotUsed

  Arg2 ::
    ConstructionOrOptimization constraint
    => Val a constraint WithArg1NotUsed (WithArg2Used a)

  LetIn1 ::
    (Show u, Show a, Arg1 arg1Root, Arg2 arg2Root)
    => Val u constraint arg1Root arg2Root
    -> Val a constraint (WithArg1Used u) WithArg2NotUsed
    -> Val a constraint arg1Root arg2Root

  LetIn2 ::
    (Show u, Show v, Show a,
     MergeUsedArg1 arg1RootA arg1RootB arg1RootMerged,
     MergeUsedArg2 arg2RootA arg2RootB arg2RootMerged)
    => Val u constraint arg1RootA arg2RootA
    -> Val v constraint arg1RootB arg2RootB
    -> Val a constraint (WithArg1Used u) (WithArg2Used v)
    -> Val a constraint arg1RootMerged arg2RootMerged

  AssocMul ::
    (Arg1 arg1, Arg2 arg2)
    => OpSide
    -> Val E.Exp GeneralUse arg1 arg2
    -> Val E.Exp GeneralUse arg1 arg2

  TransformMul ::
    OpSide
    -> Val E.Exp GeneralUse (WithArg1Used E.Exp) WithArg2NotUsed
    -> Val E.Exp GeneralUse (WithArg1Used E.Exp) WithArg2NotUsed

  Subst ::
    (Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => E.Exp
    -> Val Int GeneralUse arg1A arg2A
    -> Val E.Exp GeneralUse arg1B arg2B
    -> Val E.Exp GeneralUse arg1Merged arg2Merged

  Recurse ::
    (ConstructionOrOptimization constraint,
     ConstructionOrOptimization intConstraint,
     Arg1 arg1, Arg2 arg2)
    => Val a constraint (WithArg1Used a) WithArg2NotUsed
    -> Val Int intConstraint arg1 arg2
    -> Val a constraint arg1 arg2

  UnOp ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     Arg1 arg1, Arg2 arg2)
    => UnOp a b
    -> Val a constraint arg1 arg2
    -> Val b constraint arg1 arg2

  BinOp ::
    (Show a, ConstructionOrOptimization constraint,
     Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => BinOp a constraint
    -> Val a constraint arg1A arg2A
    -> Val a constraint arg1B arg2B
    -> Val a constraint arg1Merged arg2Merged

  Apply ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     Arg1 arg1, Arg2 arg2)
    => Val b constraint (WithArg1Used a) WithArg2NotUsed
    -> Val a constraint arg1 arg2
    -> Val b constraint arg1 arg2

  ApplyCalculated ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => Val b constraint arg1A arg2A
    -> Val a constraint arg1B arg2B
    -> Val b constraint arg1Merged arg2Merged

  ApplyFun ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => Val (Fun a b) constraint arg1A arg2A
    -> Val a constraint arg1B arg2B
    -> Val b constraint arg1Merged arg2Merged

  -- FunVal ::
  --   ConstructionOrOptimization constraint
  --   => Fun a b
  --   -> Val (Fun a b) constraint WithArg1NotUsed WithArg2NotUsed

  Compose ::
    (Show a, Show b, Show c, ConstructionOrOptimization constraint,
     Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg1 arg1A arg1B arg1Merged,
     MergeUsedArg2 arg2A arg2B arg2Merged)
    => Val (Fun a b) constraint arg1A arg2A
    -> Val (Fun b c) constraint arg1B arg2B
    -> Val (Fun a c) constraint arg1Merged arg2Merged

deriving instance Show a => Show (Val a constraint arg1 arg2)

calculated :: Val a constraint WithArg1NotUsed WithArg2NotUsed
           -> Val a constraint (WithArg1Used a) WithArg2NotUsed
calculated = unsafeCoerce -- fixme

limit :: Val a GeneralUse arg1 arg2 -> Val a ConstructionOnly arg1 arg2
limit = unsafeCoerce -- fixme

formatVal :: Show a => Val a constraint arg1 arg2 -> String
formatVal = intercalate "\n" . formatVal'
  where formatVal' :: Show a => Val a constraint arg1 arg2 -> [String]
        formatVal' = \case
          Lit x -> arg "Lit" [show x]
          LitFun (Fun v) -> arg "LitFun" (formatVal' v)
          Tup x y -> arg "Tup" (formatVal' x ++ formatVal' y)
          Fst x -> arg "Fst" (formatVal' x)
          Snd x -> arg "Snd" (formatVal' x)
          ArgN -> ["ArgN"]
          ArgV -> ["ArgV"]
          Arg1 -> ["Arg1"]
          Arg2 -> ["Arg2"]
          LetIn1 x res -> arg "LetIn1" (formatVal' x ++ formatVal' res)
          LetIn2 x y res -> arg "LetIn2" (formatVal' x ++ formatVal' y ++ formatVal' res)
          AssocMul opSide x -> arg "AssocMul" (show opSide : formatVal' x)
          TransformMul opSide x -> arg "TransformMul" (show opSide : formatVal' x)
          Subst e x y -> arg "Subst" (("EXP[" ++ E.formatExp e ++ "]") : formatVal' x ++ formatVal' y)
          Recurse x y -> arg "Recurse" (formatVal' x ++ formatVal' y)
          UnOp op x -> arg "UnOp" (show op : formatVal' x)
          BinOp op x y -> arg "UnOp" (show op : formatVal' x ++ formatVal' y)
          Apply f x -> arg "Apply" (formatVal' f ++ formatVal' x)
          ApplyCalculated f x -> arg "ApplyCalculated" (formatVal' f ++ formatVal' x)
          ApplyFun f x -> arg "ApplyFun" (formatVal' f ++ formatVal' x)
          Compose x y -> arg "Compose" (formatVal' x ++ formatVal' y)
          where arg :: String -> [String] -> [String]
                arg s ts = (s ++ "(") : map (" " ++) ts ++ [")"]
