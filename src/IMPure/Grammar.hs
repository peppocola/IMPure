-- Grammar.hs contains the structures for the internal representation of
-- the data.
module IMPure.Grammar where

type Program = [Command]

data Command
  = VariableDeclaration String AExp
  | Assignment String AExp
  | IfThenElse BExp [Command] [Command]
  | While BExp [Command]
  | Skip
  deriving Show
{--
  instance Show Command where
  show (VariableDeclaration s a) = "new variable: " ++ show s ++ ":=" ++ show a ++ "\n"
  show (Assignment s a) = show s ++ "=" ++ show a ++ "\n"
  show (IfThenElse b c c1) = "if" ++ show b ++ "\nthen :\n" ++ show c ++ "\nelse :\n" ++ show c1
  show (While b c) = "while " ++ show b ++ "\n" ++ "do : \n" ++ show c
  show Skip = "\nskip\n" 
--}

data AExp
  = Constant Int
  | AVariable String
  | Add AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp

instance Show AExp where
  show (Constant a) = show a
  show (AVariable a) = show a
  show (Add a b) = show a ++ "+" ++ show b
  show (Sub a b) = show a ++ "-" ++ show b
  show (Mul a b) = show a ++ "*" ++ show b

data BExp
  = Boolean Bool
  | Not BExp
  | Or BExp BExp
  | And BExp BExp
  | Comparison AExp AExp Operator
  deriving Show
{--
  instance Show BExp where
  show (Boolean b) = show b
  show (Not b) = "not " ++ show b
  show (Or b b1) = show b ++ " or " ++ show b1
  show (And b b1) = show b ++ " and " ++ show b1
  show (Comparison a a1 op) = show a ++ show op ++ show a1
--}
data Operator
  = Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
  deriving Show
{--
instance Show Operator where
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="
  show Eq = "=="
  show Neq = "!="
--}