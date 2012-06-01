-- Exprs.hs
module Exprs where

type Var = String

data Operator 
    = Plus | Minus | Times | Div | Mod | Neg    -- arithmetic 
    | Equal | NotEqual | Less | Greater | LessEq | GreaterEq  -- relational
    | True | False | And | Or | Not   -- boolean
    deriving (Show, Eq)

data Expr
    = Nat Int
    | Id String
    | UnaryApp Operator Expr
    | BinaryApp Operator Expr Expr
    | If Expr Expr Expr
    | App Var Expr
    deriving (Show, Eq)

data FunExpr = Fn Var Expr
    deriving (Show, Eq)

data Decl 
    = Let Var Expr
    | Fun Var Var Expr
    deriving (Show, Eq)

data Stmt = D Decl | E Expr
type Prog = [Stmt]
