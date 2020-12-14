module IMPure.Parser where

import IMPure.Grammar (AExp (..), BExp (..), Command (..), Operator (..))

newtype Parser a = P (String -> Maybe (a, String))

-- Main function that executes the parsing, given a string written in IMPure language
parse :: String -> ([Command], String)
parse s = case p s of
  Nothing -> ([],"")
  Just (c, s) -> (c, s) 
  where (P p) = program
  
 {-- (first, second)
  where
    (P p) = program
    result = p s
    first = fst (head result)
    second = snd (head result)
--}
parseFailed :: ([Command], String) -> Bool
parseFailed (_ , "") = False
parseFailed (_ , _) = True

getParsedCommands :: ([Command], String) -> [Command]
getParsedCommands (c , _) = c

getRemainingInput :: ([Command], String) -> String
getRemainingInput (_ , s) = s

instance Functor Parser where
  fmap g (P p) =
    P
      ( \input -> case p input of
          Nothing -> Nothing
          Just (v, out) -> Just (g v, out)
      )

instance Applicative Parser where
  pure v = P (\input -> Just (v, input))
  (P pg) <*> px =
    P
      ( \input -> case pg input of
          Nothing -> Nothing
          Just (g, out) -> case fmap g px of
            (P p) -> p out
      )

instance Monad Parser where
  (P p) >>= f =
    P
      ( \input -> case p input of
          Nothing -> Nothing
          Just (v, out) -> case f v of
            (P p) -> p out
      )

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]
  many x = some x <|> pure []
  some x = (:) <$> x <*> many x

--  many x = (:) <$> x <*> many x <|> pure []
--  some x = (:) <$> x <*> (some x <|> pure [])

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> my = my
  (Just x) <|> _ = Just x

instance Alternative Parser where
  empty = P (const Nothing)

  (P p) <|> (P q) =
    P
      ( \input -> case p input of
          Nothing -> q input
          Just (v, out) ->Just (v, out)
      )

-- Parses a single char
item :: Parser Char
item =
  P
    ( \inp -> case inp of
        [] -> Nothing
        (x : xs) -> Just (x, xs)
    )

-- Given a property p it verifies if the property holds for x
sat :: (Char -> Bool) -> Parser Char
sat p =
  do
    x <- item
    if p x then return x else empty

spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace c = elem c spaces

-- Removes whitespaces
space :: Parser ()
space =
  do
    many (sat isSpace)
    return ()

digits :: [Char]
digits = ['0' .. '9']

isDigit :: Char -> Bool
isDigit c = elem c digits

digit :: Parser Char
digit = sat isDigit

lowers :: [Char]
lowers = ['a' .. 'z']

isLower :: Char -> Bool
isLower c = elem c lowers

lower :: Parser Char
lower = sat isLower

uppers :: [Char]
uppers = ['A' .. 'Z']

isUpper :: Char -> Bool
isUpper c = elem c uppers

upper :: Parser Char
upper = sat isUpper

isLetter :: Char -> Bool
isLetter c = isUpper c || isLower c

letter :: Parser Char
letter = sat isLetter

isAlphaNum :: Char -> Bool
isAlphaNum c = isLetter c || isDigit c

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x : xs) =
  do
    char x
    string xs
    return (x : xs)

anIdentifier :: Parser String
anIdentifier =
  do
    x <- letter
    xs <- many alphanum
    return (x : xs)

aNaturalNumber :: Parser Int
aNaturalNumber =
  do
    xs <- some digit
    return (read xs)

int :: Parser Int
int =
  do
    char '-'
    n <- naturalNumber
    return (- n)
    <|> naturalNumber

token :: Parser a -> Parser a
token p =
  do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token anIdentifier

naturalNumber :: Parser Int
naturalNumber = token aNaturalNumber

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

aexp :: Parser AExp
aexp =
  do
    a <- aTerm
    do
      symbol "+"
      Add a <$> aexp
      <|> do
        symbol "-"
        Sub a <$> aexp
      <|> return a

aTerm :: Parser AExp
aTerm =
  do
    a <- aFactor
    do
      symbol "*"
      Mul a <$> aTerm
      <|> return a

aFactor :: Parser AExp
aFactor =
  (Constant <$> integer)
    <|> (AVariable <$> identifier)
    <|> do
      symbol "("
      a <- aexp
      symbol ")"
      return a

bexp :: Parser BExp
bexp =
  do
    b <- bTerm
    symbol "or"
    Or b <$> bexp
    <|> do
      comparison

bTerm :: Parser BExp
bTerm =
  do
    b <- bFact
    symbol "and"
    Or b <$> bexp

bFact :: Parser BExp
bFact =
  do
    symbol "True"
    return (Boolean True)
    <|> do
      symbol "False"
      return (Boolean False)
    <|> do
      symbol "not"
      Not <$> bexp
    <|> do
      symbol "("
      b <- bexp
      symbol ")"
      return b

comparison :: Parser BExp
comparison =
  do
    a1 <- aexp
    do
      symbol "<"
      a2 <- aexp
      return (Comparison a1 a2 Lt)
      <|> do
        symbol "<="
        a2 <- aexp
        return (Comparison a1 a2 Le)
      <|> do
        symbol ">"
        a2 <- aexp
        return (Comparison a1 a2 Gt)
      <|> do
        symbol ">="
        a2 <- aexp
        return (Comparison a1 a2 Ge)
      <|> do
        symbol "=="
        a2 <- aexp
        return (Comparison a1 a2 Eq)
      <|> do
        symbol "!="
        a2 <- aexp
        return (Comparison a1 a2 Neq)

command :: Parser Command
command =
  variableDeclaration
    <|> assignment
    <|> ifThenElse
    <|> while
    <|> skip

program :: Parser [Command]
program =
  do many command

variableDeclaration :: Parser Command
variableDeclaration =
  do
    symbol "var"
    i <- identifier
    symbol "="
    r <- VariableDeclaration i <$> aexp
    symbol ";"
    return r

assignment :: Parser Command
assignment =
  do
    i <- identifier
    symbol "="
    r <- Assignment i <$> aexp
    symbol ";"
    return r

skip :: Parser Command
skip =
  do
    symbol "skip"
    symbol ";"
    return Skip

ifThenElse :: Parser Command
ifThenElse =
  do
    symbol "if"
    symbol "("
    b <- bexp
    symbol ")"
    symbol "{"
    thenProgram <- program
    do
      symbol "}"
      symbol "else"
      symbol "{"
      elseProgram <- program
      symbol "}"
      return (IfThenElse b thenProgram elseProgram)
      <|> do
        symbol "}"
        return (IfThenElse b thenProgram [Skip])

while :: Parser Command
while =
  do
    symbol "while"
    symbol "("
    b <- bexp
    symbol ")"
    symbol "{"
    p <- program
    symbol "}"
    return (While b p)