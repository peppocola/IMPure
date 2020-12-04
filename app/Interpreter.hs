module Interpreter where

import Dict (Dict, empty, insert, get)
import Grammar

type Env = Dict String Int

emptyState:: Env
emptyState = empty

aexpEval:: Env -> AExp -> Maybe Int
aexpEval _ (Constant i) = Just i
aexpEval e (AVariable s) = get e s --Only way to reach Nothing if the variable is not in the Env
aexpEval e (Add a b) = pure (+) <*> (aexpEval e a) <*> (aexpEval e b) --Applicative
aexpEval e (Sub a b) = pure (-) <*> (aexpEval e a) <*> (aexpEval e b)
aexpEval e (Mul a b) = pure (*) <*> (aexpEval e a) <*> (aexpEval e b)

bexpEval:: Env -> BExp -> Maybe Bool
bexpEval _ (Boolean b) = Just b
bexpEval e (Not b) = not <$> bexpEval e b --Functor
bexpEval e (Or a b) = pure (||) <*> (bexpEval e a) <*> (bexpEval e b) --Applicative
bexpEval e (And a b) = pure (&&) <*> (bexpEval e a) <*> (bexpEval e b)
bexpEval e (Comparison a b op) = compEval e a b op 

compEval:: Env -> AExp -> AExp -> Operator -> Maybe Bool
compEval e a b (Lt) = pure (<) <*> (aexpEval e a) <*> (aexpEval e b)
compEval e a b (Le) = pure (<=) <*> (aexpEval e a) <*> (aexpEval e b)
compEval e a b (Gt) = pure (>) <*> (aexpEval e a) <*> (aexpEval e b)
compEval e a b (Ge) = pure (>=) <*> (aexpEval e a) <*> (aexpEval e b)
compEval e a b (Eq) = pure (==) <*> (aexpEval e a) <*> (aexpEval e b)
compEval e a b (Neq) = pure (/=) <*> (aexpEval e a) <*> (aexpEval e b)

commandsExec:: Env -> [Command] -> Env
commandsExec e [] = e
commandsExec e (Skip : cs) = commandsExec e cs
commandsExec e ((VariableDeclaration s ex) : cs) = 
    case aexpEval e ex of 
        Just ex' -> commandsExec (insert e s ex') cs -- Multiple declaration of same variable allowed since there's no check
        Nothing -> commandsExec e cs -- Dumb assumption
commandsExec e ((Assignment s ex) : cs) =
    case get e s of
        Just v -> commandsExec (insert e s ex') cs -- variable present
            where Just ex' = aexpEval e ex
        Nothing -> commandsExec e cs -- undecleared variable (Dumb assumption) ignore assignment
commandsExec e ((IfThenElse b nc nc') : cs) = 
    case bexpEval e b of
        Just True -> commandsExec e (nc ++ cs)
        Just False -> commandsExec e (nc'++ cs)
        Nothing -> commandsExec e cs --bexp is invalid so we should propagate error but we ignore (Dumb assumption)
commandsExec e ((While b c) : cs) =
    case bexpEval e b of
        Just True -> commandsExec e (c ++ [(While b c)] ++ cs)
        Just False -> commandsExec e cs
        Nothing -> commandsExec e cs --bexp is invalid so we should propagate error but we ignore (Dumb assumption)

programExec:: Env -> Program -> Env
programExec e (Program c) = commandsExec e c