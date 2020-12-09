<p align="center" width="100%">
<img src="img/IMPure.jpg" width="350">
</p>

# 😈IMPure😈
A simple interpreter for the IMP language written in Haskell.
This parser-interpreter was realized for the course of "**Formal Method for Computer Science**" by Giuseppe Colavito.

***WARNING***
If you are here for the Formal Methods for Computer Science exam, this implementation is not suitable for your exam, since I use some libraries external to prelude. If you want to see my exam project, switch to the *JustPrelude* branch!
***END WARNING***

IMP is a simple imperative language. It is composed by these basic structures:
<ul>
<li>Assignment : assign a value to a variable</li> 
<li>If then else : if a boolean expression is true, then some instructions are executed. If not, some other instruction are executed</li>
<li>While : loops executing the same command while a boolean condition is true</li>
<li>Skip : does nothing</li>
</ul>

The IMPure interpreter uses eager evaluation strategy. To perform this kind of execution the interpreter uses the **call-by-value**.
The IMPure language can only accept variables of type Integer. 
# Grammar
Here is reported the formal grammar of the IMPure language.
```EBNF
program ::=   <command>
          |   <command> <program>

command ::=   <assignment> ";"
          |   <ifThenElse> ";"
          |   <while> ";"
          |   <skip> ";"

assignment ::=    <identifier> "=" <aexp>
            |     <identifier> "=" <bexp>

ifThenElse ::=    "if" (<bexp>) "{" <program> "}"
            |     "if" (<bexp>) "{" <program> "}" "else" "{" <program> "}"

while ::=      "while" (<bexp>) "{" <program> "}"

skip ::=   "skip"

aexp ::=    <aterm>
      |     <aterm> "+" <aexp>
      |     <aterm> "-" <aexp>
      |     <aterm> "*" <aexp>
      |     <aterm> "/" <aexp>

aterm ::=   <positiveterm>
      |     <negativeterm>

negativeterm ::=    "-" <positiveterm>

positiveterm ::=    <positivenumber>
              |     <identifier>

bexp ::=        <truthvalue>
      |         "not" <bexp>
      |         <bexp> "or" <bexp>
      |         <bexp> "and" <bexp>
      |         <aexp> <operator> <aexp>

truthvalue ::=    "True"
            |     "False"
            |     <identifier>

operator ::=    "<"
          |     ">"
          |     "=="
          |     "<="
          |     ">="
          |     "!="

integer ::=   <digit>
          |   <digit> <integer>

digit ::=     [0-9]*

identifier ::=    [a-zA-Z_][a-zA-Z_0-9]*
```

# Design 
The IMPure interpreter is splitted in two part:
<ul>
<li>A parser</li>
<li>An intepreter</li>
</ul>

The input file is passed to the parser, who creates an internal representation of the program.
The output of the parser is then passed to the interpreter that evaluates the program and updates the state of the memory that is empty at the start of the interpretation step. When the interpreter encounters a name of variable, he goes check into the state of the memory and uses the value of the variable.

# Implementation
The environment (the state of the memory) is defined as a dictionary, where is stored the value associated with each name of variable that is declarated (and eventually updated) in the program.

```Haskell
type Env = Dict String Int
```
### Environment Management
The environment must be kept updated within the execution of the program. For this purpose, the basic operation of the dictionary are used.

```Haskell
module IMPure.Dict where

newtype Dict key value = Dict [(key, value)]

--get an empty dictionary
empty :: (Eq key) => Dict key value
empty = Dict []

--check if a dictionary is empty
isempty :: (Eq key) => Dict key value -> Bool
isempty (Dict []) = True
isempty _ = False

--get the value for a given key
get :: (Eq key) => Dict key value -> key -> Maybe value
get (Dict []) _ = Nothing
get (Dict ((k, v) : ps)) key =
  if key == k
    then Just v
    else get (Dict ps) key

--insert into dictionary
insert :: (Eq key) => Dict key value -> key -> value -> Dict key value
insert (Dict []) key value = Dict [(key, value)]
insert (Dict ((k, v) : ps)) key value =
  if key == k
    then Dict ((key, value) : ps)
    else Dict ((k, v) : ds)
  where
    (Dict ds) = insert (Dict ps) key value --unwrap ds from dictionary
```

The interpreter operates on the internal representation of the program that is constructed from the code by the parser.

### Internal structures

The internal structures used for this purpose are similar to the grammar's non-terminals : 

```Haskell
type Program = [Command]
```
The program is represented as a list of commands.

```Haskell
data Command
  = VariableDeclaration String AExp
  | Assignment String AExp
  | IfThenElse BExp [Command] [Command]
  | While BExp [Command]
  | Skip
```

The available commands are:
<ul>
<li>Variable declaration, to declare a variable and assign to it an integer value,</li>
<li>Assignment, to assign to a previously declared variable a new value,</li>
<li>If-then-else, which executes the first list of commands if the boolean condition is true, otherwhise it executes the second list of commands, </li>
<li>While, which executes the list of commands while the boolean condition is true,</li>
<li>Skip, which goes to the next command without doing anything.</li>
</ul>

```Haskell
data AExp
  = Constant Int
  | AVariable String
  | Add AExp AExp
  | Sub AExp AExp
  | Mul AExp AExp
```
An arithmetic expression could be an integer constant, a name of variable, or an operation between two arithmetic expressions.
The available operations on arithmetic expressions are addition, subtraction and multiplication.

```Haskell
data BExp
  = Boolean Bool
  | Not BExp
  | Or BExp BExp
  | And BExp BExp
  | Comparison AExp AExp Operator

data Operator
  = Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Neq
```
A boolean expression could be a boolean constant, an operation on boolean expression or a comparison between arithmetic expressions.
The available operations on boolean expressions are not, or and and.
The available comparison operators are less-then, less-equal, greater-then, greater-equal, equal, not-equal.

### Interpreter Implementation
To implement all of the constructs of the IMP language, the interpreter will have to evaluate arithmetic expressions, boolean expressions and the commands we talked about (eg. if, while, ...). 
The results of the evaluation  of the interpreter are wrapped in a *Result* type (similar to Maybe), which is defined this way:
```Haskell
import Control.Exception (Exception)
import Text.Printf (printf)

data Result a = Legal a | Error InterpreterException

instance Functor Result where
  fmap f (Legal r) = Legal (f r)
  fmap _ (Error e) = Error e

instance Applicative Result where
  pure r = Legal r
  (<*>) (Legal f) (Legal r) = Legal (f r)
  (<*>) (Error e) _ = Error e
  (<*>) _ (Error e) = Error e

data InterpreterException
  = UndeclearedVariable String
  | MultipleDeclaration String
  | InvalidBExp String
  | InvalidAExp String

instance Show InterpreterException where
  show (UndeclearedVariable s) = printf "Undecleared variable! '%s'" s
  show (MultipleDeclaration s) = printf "Multiple declaration of a variable! '%s'" s
  show (InvalidBExp s) = printf "The boolean expression is invalid! '%s'" s
  show (InvalidAExp s) = printf "The arithmetic expression is invalid! '%s'" s

instance Exception InterpreterException
```
With this implementation we can easly throw exception when they are encountered in the interpreter to better understand which part of the code caused the error! With the help of the Functor and Applicative that has been implemented for the Result type, we can easly unwrap the results, do some operation on them and then wrap them again and return it.

#### Arithmetic expression evaluation
The interpreter can evaluate an arithmetic expression given an environment and an *AExp* (that is defined in the internal structures). The output can be a ```Legal Int``` or an ```Error```. This evaluation is implemented using Functor(<$>) and Applicative (<*>).

```Haskell
aexpEval :: Env -> AExp -> Result Int
aexpEval _ (Constant i) = Legal i
aexpEval e (AVariable s) =
  case get e s of
    Just v -> Legal v
    Nothing -> Error (UndeclearedVariable s) 
aexpEval e (Add a b) = (+) <$> aexpEval e a <*> aexpEval e b
aexpEval e (Sub a b) = (-) <$> aexpEval e a <*> aexpEval e b
aexpEval e (Mul a b) = (*) <$> aexpEval e a <*> aexpEval e b
```

#### Boolean expression evaluation
The interpreter can evaluate a boolean expression given an environment and a *BExp* (that is defined in the internal structures). The output can be a ```Legal Bool``` or an ```Error```. This evaluation is implemented again using Functor(```<$>```) and Applicative (```<*>```).
```Haskell
bexpEval :: Env -> BExp -> Result Bool
bexpEval _ (Boolean b) = Legal b
bexpEval e (Not b) = not <$> bexpEval e b
bexpEval e (Or a b) = (||) <$> bexpEval e a <*> bexpEval e b
bexpEval e (And a b) = (&&) <$> bexpEval e a <*> bexpEval e b
bexpEval e (Comparison a b op) = compEval e a b op

compEval :: Env -> AExp -> AExp -> Operator -> Result Bool
compEval e a b Lt = (<) <$> aexpEval e a <*> aexpEval e b
compEval e a b Le = (<=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Gt = (>) <$> aexpEval e a <*> aexpEval e b
compEval e a b Ge = (>=) <$> aexpEval e a <*> aexpEval e b
compEval e a b Eq = (==) <$> aexpEval e a <*> aexpEval e b
compEval e a b Neq = (/=) <$> aexpEval e a <*> aexpEval e b
```
#### Commands Execution
Given an environment and a list of commands, we can execute the commands in the list (the program).
```Haskell
programExec :: Env -> [Command] -> Env
programExec e [] = e
programExec e (Skip : cs) = programExec e cs
programExec e ((VariableDeclaration s ex) : cs) =
  case aexpEval e ex of
    Legal ex' -> case get e s of
      Just _ -> throw (MultipleDeclaration s)
      Nothing -> programExec (insert e s ex') cs
    Error er -> throw er
programExec e ((Assignment s ex) : cs) =
  case get e s of
    Just _ -> programExec (insert e s ex') cs
      where
        Legal ex' = aexpEval e ex
    Nothing -> throw (UndeclearedVariable s)
programExec e ((IfThenElse b nc nc') : cs) =
  case bexpEval e b of
    Legal True -> programExec e (nc ++ cs)
    Legal False -> programExec e (nc' ++ cs)
    Error er -> throw er
programExec e ((While b c) : cs) =
  case bexpEval e b of
    Legal True -> programExec e (c ++ [While b c] ++ cs)
    Legal False -> programExec e cs
    Error er -> throw er
```
And here is our interpreter. If we give in input a program (written as the internal representation of the program of the interpreter), the interpreter will evaluate the program and give us in output the state of the memory at the end of the program!
If something goes wrong, an exception is thrown and the execution gets interrupted. Some basic information about the error are shown.

This is how a legal input for the interpreter looks like. At the end of the computation, x will be the result of the factorial of 5.

```Haskell
          [ VariableDeclaration "i" (Constant 1),
            VariableDeclaration "n" (Constant 5),
            VariableDeclaration "x" (Constant 1),
            While
              (Comparison (AVariable "i") (AVariable "n") Le)
              [ Assignment "x" (Mul (AVariable "x") (AVariable "i")),
                Assignment "i" (Add (AVariable "i") (Constant 1))
              ]
          ]
```
Of course this "language" is too hard to write and understand. For more complex programs it would be really heavy to read and write.
To avoid this problems we define a more friendly language and implement a parser that will transform the new language in the language of the interpreter.
 
### Parser Implementation

# Execution Example
In this example the IMPure interpreter evaluates the factorial (the code used can be found in the file **test.pure**)
![](img/example.gif)
