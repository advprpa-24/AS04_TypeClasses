{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall #-}
module Lang.Imp where

import qualified Map.TreeMap as M

todo :: a
todo = error "TODO"


type Name = String
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    BNot :: Expr Bool -> Expr Bool
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool

    REq :: Expr Integer -> Expr Integer -> Expr Bool
    RLt :: Expr Integer -> Expr Integer -> Expr Bool

    AConst :: Integer -> Expr Integer
    AVar   :: Name -> Expr Integer
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer
    ADiv   :: Expr Integer -> Expr Integer -> Expr Integer
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer

deriving instance Eq (Expr a) 
deriving instance Show (Expr a)

data Cmd = 
    CSkip
  | CAssign Name (Expr Integer)
  | CIfThEl (Expr Bool) Cmd Cmd
  | CWhile (Expr Bool) Cmd
  | CSeq [Cmd]

eval :: Expr a -> State -> a
eval (BNot b) st = not (eval b st)
eval _ _ = todo

exec :: Cmd -> State -> State
exec CSkip st = st
exec _ _ = todo

