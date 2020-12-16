-- Grammar.hs contains the structures for the internal representation of
-- the data.
module IMPure.Grammar where
import IMPure.Dict (Dict)

data Type = IntType Int | BoolType Bool | ArrayType [Int]
  deriving Show
type Program = [Command]

data Command
  = AeVariableDeclaration String AExp
  | BeVariableDeclaration String BExp
  | ArVariableDeclaration String AExp
  | AeAssignment String AExp
  | BeAssignment String BExp
  | ArAssignment String AExp AExp
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
  | AArray String AExp
  | Add AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
  deriving Show
{--
instance Show AExp where
  show (Constant a) = show a
  show (AVariable a) = show a
  show (AArray s i) = "(Array " ++ show s ++" position "++ show i ++ ")"
  show (Add a b) = show a ++ " + " ++ show b
  show (Sub a b) = show a ++ " - " ++ show b
  show (Mul a b) = show a ++ " * " ++ show b
--}
data BExp
  = Boolean Bool
  | BVariable String
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