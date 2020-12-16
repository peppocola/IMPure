module IMPure.Interpreter where
import IMPure.Dict (Dict, empty, get, insert)
import IMPure.Grammar
  ( AExp (..),
    BExp (..),
    Command (..),
    Operator (..),
    Type (..)
  )
import IMPure.Array ( declareArray, writeArray, readArray)

type Env = Dict String Type

emptyState :: Env
emptyState = empty

aexpEval :: Env -> AExp -> Maybe Int
aexpEval _ (Constant i) = Just i
aexpEval e (AVariable s) =
  case get e s of
    Just (IntType v) -> Just v
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
aexpEval e (AArray s i) =
  case get e s of
    Just (ArrayType a) -> Just (readArray a j)
      where Just j = aexpEval e i 
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
aexpEval e (Add a b) = (+) <$> aexpEval e a <*> aexpEval e b --Applicative
aexpEval e (Sub a b) = (-) <$> aexpEval e a <*> aexpEval e b
aexpEval e (Mul a b) = (*) <$> aexpEval e a <*> aexpEval e b

bexpEval :: Env -> BExp -> Maybe Bool
bexpEval _ (Boolean b) = Just b
bexpEval e (BVariable s) =
  case get e s of
    Just (BoolType v) -> Just v
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
bexpEval e (Not b) = not <$> bexpEval e b --Functor
bexpEval e (Or a b) = (||) <$> bexpEval e a <*> bexpEval e b --Applicative
bexpEval e (And a b) = (&&) <$> bexpEval e a <*> bexpEval e b
bexpEval e (Comparison a b op) = compEval e a b op

compEval :: Env -> AExp -> AExp -> Operator -> Maybe Bool
compEval e a b Lt = (<) <$> aexpEval e a <*> aexpEval e b
compEval e a b Le = (<=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Gt = (>) <$> aexpEval e a <*> aexpEval e b
compEval e a b Ge = (>=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Eq = (==) <$> aexpEval e a <*> aexpEval e b
compEval e a b Neq = (/=) <$> aexpEval e a <*> aexpEval e b

programExec :: Env -> [Command] -> Env
programExec e [] = e
programExec e (Skip : cs) = programExec e cs
programExec e ((AeVariableDeclaration s ex) : cs) =
  case aexpEval e ex of
    Just ex' -> case get e s of
      Just _ -> error "MultipleDeclaration"
      Nothing -> programExec (insert e s (IntType ex')) cs
    Nothing -> error "InvalidArithmeticExpression"
programExec e ((BeVariableDeclaration s ex) : cs) =
  case bexpEval e ex of
    Just ex' -> case get e s of
      Just _ -> error "MultipleDeclaration"
      Nothing -> programExec (insert e s (BoolType ex')) cs
    Nothing -> error "InvalidBooleanExpression"
programExec e ((ArVariableDeclaration s i) : cs) =
  case get e s of
    Just _ -> error "MultipleDeclaration"
    Nothing -> programExec (insert e s (ArrayType (declareArray j))) cs
    where Just j = aexpEval e i
programExec e ((AeAssignment s ex) : cs) =
  case get e s of
    Just (IntType _) -> programExec (insert e s (IntType ex')) cs
      where
        Just ex' = aexpEval e ex
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
programExec e ((BeAssignment s ex) : cs) =
  case get e s of
    Just (BoolType _) -> programExec (insert e s (BoolType ex')) cs
      where
        Just ex' = bexpEval e ex
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
programExec e ((ArAssignment s i ex) : cs) =
  case get e s of
    Just (ArrayType a) -> programExec (insert e s (ArrayType (writeArray a j ex'))) cs
      where
        Just ex' = aexpEval e ex
        Just j = aexpEval e i
    Just _ -> error "TypeMismatch"
    Nothing -> error "UndeclearedVariable"
programExec e ((IfThenElse b nc nc') : cs) =
  case bexpEval e b of
    Just True -> programExec e (nc ++ cs)
    Just False -> programExec e (nc' ++ cs)
    Nothing -> error "InvalidBooleanExpression"
programExec e ((While b c) : cs) =
  case bexpEval e b of
    Just True -> programExec e (c ++ [While b c] ++ cs)
    Just False -> programExec e cs
    Nothing -> error "InvalidBooleanExpression"