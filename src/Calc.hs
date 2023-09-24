{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
module Calc ( GeneralUse
            , ConstructionOnly
            , ConstructionOrOptimization
            -- , Arg1
            -- , Arg2
            -- , WithArg1Used
            -- , WithArg1NotUsed
            -- , WithArg2Used
            -- , WithArg2NotUsed
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
import Data.Kind (Type)
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

-- data WithArg1Used a
-- data WithArg1NotUsed
-- data WithArg2Used a
-- data WithArg2NotUsed

-- -- data ArgUsedKind = ArgUsed | ArgNotUsed
-- --   deriving (Show)

-- class Arg1 a
-- instance Arg1 (WithArg1Used a)
-- instance Arg1 WithArg1NotUsed

-- class Arg2 a
-- instance Arg2 (WithArg2Used a)
-- instance Arg2 WithArg2NotUsed

-- class MergeUsedArg1 a b m | a b -> m
-- class MergeUsedArg2 a b m | a b -> m

-- instance MergeUsedArg1 WithArg1NotUsed WithArg1NotUsed WithArg1NotUsed
-- instance MergeUsedArg1 WithArg1NotUsed (WithArg1Used a) (WithArg1Used a)
-- instance MergeUsedArg1 (WithArg1Used a) WithArg1NotUsed (WithArg1Used a)
-- instance MergeUsedArg1 (WithArg1Used a) (WithArg1Used a) (WithArg1Used a)

-- instance MergeUsedArg2 WithArg2NotUsed WithArg2NotUsed WithArg2NotUsed
-- instance MergeUsedArg2 WithArg2NotUsed (WithArg2Used a) (WithArg2Used a)
-- instance MergeUsedArg2 (WithArg2Used a) WithArg2NotUsed (WithArg2Used a)
-- instance MergeUsedArg2 (WithArg2Used a) (WithArg2Used a) (WithArg2Used a)


class MergeUsedArg (a1 :: Arg) (t1 :: Type) (a2 :: Arg) (t2 :: Type) (am :: Arg) (tm :: Type) | a1 t1 a2 t2 -> am tm
instance MergeUsedArg 'ArgNotUsed u 'ArgNotUsed u 'ArgNotUsed u
instance MergeUsedArg 'ArgUsed a 'ArgNotUsed u 'ArgUsed a
instance MergeUsedArg 'ArgNotUsed u 'ArgUsed a 'ArgUsed a
instance MergeUsedArg 'ArgUsed a 'ArgUsed a 'ArgUsed a



-- class TopDownDetermineArg1 t a b | t -> a b
-- class TopDownDetermineArg2 t a b | t -> a b

-- instance TopDownDetermineArg1 WithArg1NotUsed WithArg1NotUsed WithArg1NotUsed
-- instance TopDownDetermineArg2 WithArg2NotUsed WithArg2NotUsed WithArg2NotUsed

data Fun a b = Fun (Val b GeneralUse 'ArgUsed a 'ArgNotUsed ())
--  deriving (Show)

data Arg = ArgUsed
         | ArgNotUsed
  deriving (Show)

data T v

data SArg (a :: Arg) where
  SArgUsed :: SArg 'ArgUsed
  SArgNotUsed :: SArg 'ArgNotUsed

deriving instance Show (SArg 'ArgUsed)
deriving instance Show (SArg 'ArgNotUsed)

-- aaa = SArgUsed (undefined :: T Int)

data Val a constraint (argUse1 :: Arg) a1 (argUse2 :: Arg) a2 where
  Lit ::
    (Show a, ConstructionOrOptimization constraint)
    => a
    -> Val a constraint 'ArgNotUsed () 'ArgNotUsed ()

  LitFun ::
    (Show a, Show b, ConstructionOrOptimization constraint)
    => Fun a b
    -> Val (Fun a b) constraint 'ArgNotUsed () 'ArgNotUsed ()

  Tup ::
    (Show a, Show b,
     -- Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     -- Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     -- TopDownDetermineArg1 arg1Merged arg1A arg1B,
     -- TopDownDetermineArg2 arg2Merged arg2A arg2B,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1Merged,
     MergeUsedArg arg2A t2A arg2B t2A arg2Merged t2Merged)
    => ((SArg arg1A, SArg arg2A), (SArg arg1B, SArg arg2B))
    -> Val a constraint arg1A t1A arg2A t2A
    -- -> ArgUsedKind
    -> Val b constraint arg1B t1B arg2B t2B
    -- -> ArgUsedKind
    -> Val (a, b) constraint arg1Merged t1Merged arg2Merged t2Merged

  Fst ::
    (Show a, Show b) -- , Arg1 arg1, Arg2 arg2)
    => Val (a, b) constraint arg1 t1 arg2 t2
    -> Val a constraint arg1 t1 arg2 t2

  Snd ::
    (Show a, Show b)
    => Val (a, b) constraint arg1 t1 arg2 t2
    -> Val b constraint arg1 t1 arg2 t2

  ArgN ::
    ConstructionOrOptimization constraint
    => Val Int constraint 'ArgNotUsed () 'ArgNotUsed ()

  ArgV ::
    ConstructionOrOptimization constraint
    => Val E.Exp constraint 'ArgNotUsed () 'ArgNotUsed ()

  Arg1 ::
    ConstructionOrOptimization constraint
    => Val a constraint 'ArgUsed a 'ArgNotUsed ()

  Arg2 ::
    ConstructionOrOptimization constraint
    => Val a constraint 'ArgNotUsed () 'ArgUsed a

  LetIn1 ::
    (Show u, Show a)
    => Val u constraint arg1 t1 arg2 t2
    -> Val a constraint 'ArgUsed u 'ArgNotUsed ()
    -> Val a constraint arg1 t1 arg2 t2

  LetIn2 ::
    (Show u, Show v, Show a,
     MergeUsedArg arg1RootA t1A arg1RootB t1B arg1RootMerged t1M,
     MergeUsedArg arg2RootA t2A arg2RootB t2B arg2RootMerged t2M)
    => Val u constraint arg1RootA t1A arg2RootA t2A
    -> Val v constraint arg1RootB t1B arg2RootB t2B
    -> Val a constraint 'ArgUsed u 'ArgUsed v
    -> Val a constraint arg1RootMerged t1M arg2RootMerged t2M

  AssocMul ::
    OpSide
    -> Val E.Exp GeneralUse arg1 t1 arg2 t2
    -> Val E.Exp GeneralUse arg1 t1 arg2 t2

  TransformMul ::
    OpSide
    -> Val E.Exp GeneralUse 'ArgUsed E.Exp 'ArgNotUsed ()
    -> Val E.Exp GeneralUse 'ArgUsed E.Exp 'ArgNotUsed ()

  Subst ::
    (-- Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     -- Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1M,
     MergeUsedArg arg2A t2A arg2B t2B arg2Merged t2M)
    => E.Exp
    -> Val Int GeneralUse arg1A t1A arg2A t2A
    -> Val E.Exp GeneralUse arg1B t1B arg2B t2B
    -> Val E.Exp GeneralUse arg1Merged t1M arg2Merged t2M

  Recurse ::
    (ConstructionOrOptimization constraint,
     ConstructionOrOptimization intConstraint)
    => Val a constraint 'ArgUsed a 'ArgNotUsed ()
    -> Val Int intConstraint arg1 t1 arg2 t2
    -> Val a constraint arg1 t1 arg2 t2

  UnOp ::
    (Show a, Show b, ConstructionOrOptimization constraint)
    => UnOp a b
    -> Val a constraint arg1 t1 arg2 t2
    -> Val b constraint arg1 t1 arg2 t2

  BinOp ::
    (Show a, ConstructionOrOptimization constraint,
     -- Arg1 arg1A, Arg1 arg1B, Arg1 arg1Merged,
     -- Arg2 arg2A, Arg2 arg2B, Arg2 arg2Merged,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1M,
     MergeUsedArg arg2A t2A arg2B t2B arg2Merged t2M)
    => BinOp a constraint
    -> Val a constraint arg1A t1A arg2A t2A
    -> Val a constraint arg1B t1B arg2B t2B
    -> Val a constraint arg1Merged t1M arg2Merged t2M

  Apply ::
    (Show a, Show b, ConstructionOrOptimization constraint)
    => Val b constraint 'ArgUsed a 'ArgNotUsed ()
    -> Val a constraint arg1 t1 arg2 t2
    -> Val b constraint arg1 t1 arg2 t2

  ApplyCalculated ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1M,
     MergeUsedArg arg2A t2A arg2B t2B arg2Merged t2M)
    => Val b constraint arg1A t1A arg2A t2A
    -> Val a constraint arg1B t1B arg2B t2B
    -> Val b constraint arg1Merged t1M arg2Merged t2M

  ApplyFun ::
    (Show a, Show b, ConstructionOrOptimization constraint,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1M,
     MergeUsedArg arg2A t2A arg2B t2B arg2Merged t2M)
    => Val (Fun a b) constraint arg1A t1A arg2A t2A
    -> Val a constraint arg1B t1B arg2B t2B
    -> Val b constraint arg1Merged t1M arg2Merged t2M

  Compose ::
    (Show a, Show b, Show c, ConstructionOrOptimization constraint,
     MergeUsedArg arg1A t1A arg1B t1B arg1Merged t1M,
     MergeUsedArg arg2A t2A arg2B t2B arg2Merged t2M)
    => Val (Fun a b) constraint arg1A t1A arg2A t2A
    -> Val (Fun b c) constraint arg1B t1B arg2B t2B
    -> Val (Fun a c) constraint arg1Merged t1M arg2Merged t2M

-- deriving instance Show a => Show (Val a constraint arg1 t1 arg2 t2)

calculated :: Val a constraint 'ArgNotUsed () 'ArgNotUsed ()
           -> Val a constraint 'ArgUsed a 'ArgNotUsed ()
calculated = unsafeCoerce -- FIXME

limit :: Val a GeneralUse arg1 t1 arg2 t2 -> Val a ConstructionOnly arg1 t1 arg2 t2
limit = unsafeCoerce -- FIXME

formatVal :: Show a => Val a constraint arg1 t1 arg2 t2 -> String
formatVal = intercalate "\n" . formatVal'
  where formatVal' :: Show a => Val a constraint arg1 t1 arg2 t2 -> [String]
        formatVal' = \case
          Lit x -> arg "Lit" [show x]
          LitFun (Fun v) -> arg "LitFun" (formatVal' v)
          -- Tup x y -> arg "Tup" (formatVal' x ++ formatVal' y)
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
          -- ApplyFun f x -> arg "ApplyFun" (formatVal' f ++ formatVal' x)
          -- Compose x y -> arg "Compose" (formatVal' x ++ formatVal' y)
          where arg :: String -> [String] -> [String]
                arg s ts = (s ++ "(") : map (" " ++) ts ++ [")"]

interpretValToExp :: Val a constraint arg1 t1 arg2 t2 -- WithArg1NotUsed WithArg2NotUsed
                  -> E.Exp -> Int -> a
interpretValToExp val argV argN = interp0 val
  where interp0 :: Val a constraint arg1 t1 arg2 t2 -- WithArg1NotUsed WithArg2NotUsed
                -> a
        interp0 = \case
          Lit x -> x
          LitFun f -> f
          Tup _ x y -> (interp0 x, interp0 y)
          Fst x -> fst $ interp0 x
          Snd x -> snd $ interp0 x
          ArgN -> argN
          ArgV -> argV
          Arg1 -> error "no arg1 in this context"
          Arg2 -> error "no arg2 in this context"
          -- LetIn1 x res -> interp1 (interp0 x) res
          -- LetIn2 x y res -> interp2 (interp0 x) (interp0 y) res
          AssocMul opSide x -> undefined
          TransformMul opSide x -> undefined
          Subst e x y -> undefined
          Recurse x y -> undefined
          UnOp op x -> undefined
          BinOp op x y -> undefined
          Apply f x -> undefined
          ApplyCalculated f x -> undefined
          ApplyFun f x -> undefined
          Compose f x -> undefined

        interp1 :: arg1
                -> Val a constraint ArgUsed arg1 ArgNotUsed arg2
                -> a
        interp1 arg1 = \case
          Lit x -> x
          LitFun f -> f
          Tup ((xArg1, xArg2), (yArg1, yArg2)) x y ->
            let xv = case (xArg1, xArg2) of
                  (SArgNotUsed, SArgNotUsed) -> interp0 x
                  (SArgUsed, SArgNotUsed) -> interp1 arg1 x
                yv = case (yArg1, yArg2) of
                  (SArgNotUsed, SArgNotUsed) -> interp0 y
                  (SArgUsed, SArgNotUsed) -> interp1 arg1 y
            in (xv, yv)
          Fst x -> fst $ interp1 arg1 x
          Snd x -> snd $ interp1 arg1 x
          ArgN -> argN
          ArgV -> argV
          Arg1 -> arg1
          Arg2 -> error "no arg2 in this context"
          -- LetIn1 x res -> interp1 (interp1 arg1 x) res
          -- LetIn2 x y res -> interp2 (interp1 arg1 x) (interp1 arg1 y) res
          AssocMul opSide x -> undefined
          TransformMul opSide x -> undefined
          Subst e x y -> undefined
          Recurse x y -> undefined
          UnOp op x -> undefined
          BinOp op x y -> undefined
          Apply f x -> undefined
          ApplyCalculated f x -> undefined
          ApplyFun f x -> undefined
          Compose f x -> undefined

        -- interp2 :: arg1
        --         -> arg2
        --         -> Val a constraint (WithArg1Used arg1) (WithArg2Used arg2)
        --         -> a
        -- interp2 arg1 arg2 = \case
        --   Lit x -> x
        --   LitFun f -> f
        --   -- Tup x y -> (interp2 arg1 arg2 x, interp2 arg1 arg2 y)
        --   Fst x -> fst $ interp2 arg1 arg2 x
        --   Snd x -> snd $ interp2 arg1 arg2 x
        --   ArgN -> argN
        --   ArgV -> argV
        --   Arg1 -> arg1
        --   Arg2 -> arg2
        --   LetIn1 x res -> interp1 (interp2 arg1 arg2 x) res
        --   LetIn2 x y res -> interp2 (interp2 arg1 arg2 x) (interp2 arg1 arg2 y) res
        --   AssocMul opSide x -> undefined
        --   TransformMul opSide x -> undefined
        --   Subst e x y -> undefined
        --   Recurse x y -> undefined
        --   UnOp op x -> undefined
        --   BinOp op x y -> undefined
        --   Apply f x -> undefined
        --   ApplyCalculated f x -> undefined
        --   ApplyFun f x -> undefined
        --   Compose f x -> undefined
