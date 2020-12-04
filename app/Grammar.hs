-- Grammar.hs contains the structures for the internal representation of
-- the data. 
module Grammar where

data Program
    = Program [Command]
    deriving Show

data Command
    = VariableDeclaration String AExp
    | Assignment String AExp
    | IfThenElse BExp [Command] [Command]
    | While BExp [Command]
    | Skip
    deriving Show

data AExp
    = Constant Int
    | AVariable String
    | Add AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
    deriving Show

data BExp
    = Boolean Bool
    | Not BExp
    | Or BExp BExp
    | And BExp BExp
    | Comparison AExp AExp Operator
    deriving Show

data Operator
    = Lt | Le | Gt | Ge | Eq | Neq
instance Show Operator where
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="
    show Eq = "=="
    show Neq = "!="