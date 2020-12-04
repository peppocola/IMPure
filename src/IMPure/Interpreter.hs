module IMPure.Interpreter where

import Control.Exception (throw)
import IMPure.Dict (Dict, empty, get, insert)
import IMPure.Exception (InterpreterException (..), Result (..))
import IMPure.Grammar
  ( AExp (..),
    BExp (..),
    Command (..),
    Operator (..),
    Program (..),
  )

type Env = Dict String Int

emptyState :: Env
emptyState = empty

aexpEval :: Env -> AExp -> Result Int
aexpEval _ (Constant i) = Legal i
aexpEval e (AVariable s) =
  case get e s of
    Just v -> Legal v
    Nothing -> Error (UndeclearedVariable s) --Only way to reach Nothing if the variable is not in the Env
aexpEval e (Add a b) = (+) <$> aexpEval e a <*> aexpEval e b --Applicative
aexpEval e (Sub a b) = (-) <$> aexpEval e a <*> aexpEval e b
aexpEval e (Mul a b) = (*) <$> aexpEval e a <*> aexpEval e b

bexpEval :: Env -> BExp -> Result Bool
bexpEval _ (Boolean b) = Legal b
bexpEval e (Not b) = not <$> bexpEval e b --Functor
bexpEval e (Or a b) = (||) <$> bexpEval e a <*> bexpEval e b --Applicative
bexpEval e (And a b) = (&&) <$> bexpEval e a <*> bexpEval e b
bexpEval e (Comparison a b op) = compEval e a b op

compEval :: Env -> AExp -> AExp -> Operator -> Result Bool
compEval e a b Lt = (<) <$> aexpEval e a <*> aexpEval e b
compEval e a b Le = (<=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Gt = (>) <$> aexpEval e a <*> aexpEval e b
compEval e a b Ge = (>=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Eq = (==) <$> aexpEval e a <*> aexpEval e b
compEval e a b Neq = (/=) <$> aexpEval e a <*> aexpEval e b

commandsExec :: Env -> [Command] -> Env
commandsExec e [] = e
commandsExec e (Skip : cs) = commandsExec e cs
commandsExec e ((VariableDeclaration s ex) : cs) =
  case aexpEval e ex of
    Legal ex' -> case get e s of
      Just _ -> commandsExec (insert e s ex') cs
      Nothing -> throw (MultipleDeclaration s)
    Error er -> throw er -- aexp is invalid
commandsExec e ((Assignment s ex) : cs) =
  case get e s of
    Just _ -> commandsExec (insert e s ex') cs
      where
        Legal ex' = aexpEval e ex
    Nothing -> throw (UndeclearedVariable s)
commandsExec e ((IfThenElse b nc nc') : cs) =
  case bexpEval e b of
    Legal True -> commandsExec e (nc ++ cs)
    Legal False -> commandsExec e (nc' ++ cs)
    Error er -> throw er
commandsExec e ((While b c) : cs) =
  case bexpEval e b of
    Legal True -> commandsExec e (c ++ [While b c] ++ cs)
    Legal False -> commandsExec e cs
    Error er -> throw er

programExec :: Env -> Program -> Env
programExec e (Program c) = commandsExec e c