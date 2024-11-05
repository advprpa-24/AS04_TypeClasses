{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module Lang.Imp_Solution where

import qualified Map.TreeMap_Solution as M
import Data.Maybe (fromMaybe)

-- Imp is a tiny imperative programming language.
-- It consists of expressions `Expr` and commands `Cmd`.

type Name = String
type State = M.Map Name Integer

emptyState :: State
emptyState = M.empty

data Expr a where
    BNot :: Expr Bool -> Expr Bool
    BAnd :: Expr Bool -> Expr Bool -> Expr Bool
    BOr  :: Expr Bool -> Expr Bool -> Expr Bool

    REq :: Eq a => Expr a -> Expr a -> Expr Bool
    RLt :: Expr Integer -> Expr Integer -> Expr Bool

    AConst :: Integer -> Expr Integer
    APlus  :: Expr Integer -> Expr Integer -> Expr Integer
    AMinus :: Expr Integer -> Expr Integer -> Expr Integer
    AMul   :: Expr Integer -> Expr Integer -> Expr Integer
    ADiv :: Expr Integer -> Expr Integer -> Expr Integer
    AMod   :: Expr Integer -> Expr Integer -> Expr Integer
    AVar   :: Name -> Expr Integer

data Cmd = 
    CSkip
  | CAssign Name (Expr Integer)
  | CIfThEl (Expr Bool) Cmd Cmd
  | CWhile (Expr Bool) Cmd
  | CSeq [Cmd]


eval :: Expr a -> State -> a
eval (BNot b)     st = not (eval b st)
eval (BAnd a b)   st = eval a st && eval b st
eval (BOr a b)    st = eval a st || eval b st

eval (REq a b)    st = eval a st == eval b st    
eval (RLt a b)    st = eval a st < eval b st

eval (AVar n)     st = fromMaybe (error $ "variable not found: " ++ n ) (M.lookup n st)
eval (AConst n)   _   = n
eval (APlus a b)  st = eval a st + eval b st
eval (AMinus a b) st = eval a st - eval b st  
eval (AMul a b)   st = eval a st * eval b st
eval (ADiv a b)   st = eval a st `div` eval b st
eval (AMod a b)   st = eval a st `mod` eval b st

exec :: Cmd -> State -> State
exec CSkip           st = st
exec (CAssign n v)   st = M.insert n (eval v st) st
exec (CIfThEl b t e) st = exec (if eval b st then t else e) st
exec (CWhile b c)    st = if eval b st then exec (CWhile b c) (exec c st) else st
exec (CSeq cs)       st = foldl (flip exec) st cs
