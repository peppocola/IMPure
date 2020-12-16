module IMPure.Parser where

import IMPure.Grammar (AExp (..), BExp (..), Command (..), Operator (..) )

newtype Parser a = P (String -> Maybe (a, String))

-- Main function that executes the parsing, given a string written in IMPure language
parse :: String -> ([Command], String)
parse s = case p s of
  Nothing -> ([], "")
  Just (c, s) -> (c, s)
  where
    (P p) = program

{-- (first, second)
  where
    (P p) = program
    result = p s
    first = fst (head result)
    second = snd (head result)
--}
parseFailed :: ([Command], String) -> Bool
parseFailed (_, "") = False
parseFailed (_, _) = True

getParsedCommands :: ([Command], String) -> [Command]
getParsedCommands (c, _) = c

getRemainingInput :: ([Command], String) -> String
getRemainingInput (_, s) = s

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

class Monad f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]
  many x = some x <|> pure []
  some x = (:) <$> x <*> many x
  chain :: f a -> f (a -> a -> a) -> f a
  chain p op = do a <- p; rest a
    where
      rest a = (do f <- op; b <- p; rest (f a b)) <|> return a

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
          Just (v, out) -> Just (v, out)
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
aexp = do chain aTerm op
  where
    op =
      (do symbol "+"; return Add)
        <|> do symbol "-"; return Sub

aTerm :: Parser AExp
aTerm = do chain aFactor op
    where op = do symbol "*"; return Mul

aFactor :: Parser AExp
aFactor =
  (Constant <$> integer)
    <|> do
      i <- identifier
      do
        symbol "["
        n <- aexp
        symbol "]"
        return (AArray i n)
        <|> return (AVariable i)
    <|> do
      symbol "("
      a <- aexp
      symbol ")"
      return a

bexp :: Parser BExp
bexp = chain bTerm op
  where op = do
            symbol "or"
            return Or

bTerm :: Parser BExp
bTerm = chain bFact op
  where op = do
            symbol "and"
            return And

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
    <|> do comparison
    <|> (BVariable <$> identifier)

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
  aeVariableDeclaration
    <|> beVariableDeclaration
    <|> arVariableDeclaration
    <|> aeAssignment
    <|> beAssignment
    <|> arAssignment
    <|> ifThenElse
    <|> while
    <|> skip

program :: Parser [Command]
program =
  do many command

aeVariableDeclaration :: Parser Command
aeVariableDeclaration =
  do
    symbol "int"
    i <- identifier
    symbol "="
    r <- AeVariableDeclaration i <$> aexp
    symbol ";"
    return r

beVariableDeclaration :: Parser Command
beVariableDeclaration =
  do
    symbol "bool"
    i <- identifier
    symbol "="
    r <- BeVariableDeclaration i <$> bexp
    symbol ";"
    return r

arVariableDeclaration :: Parser Command
arVariableDeclaration =
  do
    symbol "array"
    i <- identifier
    symbol "="
    j <- aexp
    symbol ";"
    return (ArVariableDeclaration i j)

aeAssignment :: Parser Command
aeAssignment =
  do
    i <- identifier
    symbol "="
    r <- AeAssignment i <$> aexp
    symbol ";"
    return r

beAssignment :: Parser Command
beAssignment =
  do
    i <- identifier
    symbol "="
    r <- BeAssignment i <$> bexp
    symbol ";"
    return r

arAssignment :: Parser Command
arAssignment =
  do
    i <- identifier
    symbol "["
    j <- aexp
    symbol "]"
    symbol "="
    r <- ArAssignment i j <$> aexp
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