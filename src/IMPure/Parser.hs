module IMPure.Parser where
import IMPure.Grammar (Command(..), BExp(..), Operator(..), AExp(..))

newtype Parser a = P (String -> [(a, String)])

parse :: String -> [Command]
parse s = fst(head (p s)) where (P p) = program 

instance Functor Parser where
    fmap g (P p) = P (\input -> case p input of
        [] -> []
        [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    pure v = P (\input -> [(v, input)])
    (P pg) <*> px = P (\input -> case pg input of
        [] -> []
        [(g, out)] -> case fmap g px of
            (P p) -> p out)

instance Monad Parser where
   (P p) >>= f = P(\input -> case p input of
       [] -> []
       [(v, out)] -> case f v of
           (P p) -> p out)

class Applicative f => Alternative f where
 empty :: f a
 (<|>) :: f a -> f a -> f a
 many :: f a -> f [a]
 some :: f a -> f [a]
 many x = some x <|> pure []
 some x = (:) <$> x <*> many x

instance Alternative Maybe where
 empty = Nothing
 Nothing <|> my = my
 (Just x) <|> _ = Just x

instance Alternative Parser where 
    empty = P (const [])

    (P p) <|> (P q) = P(\input -> case p input of
        [] ->  q input
        [(v, out)] -> [(v, out)])

item :: Parser Char
item = P (\inp -> case inp of 
        [] -> []
        (x:xs) -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = 
 do {
 x <- item;
 if p x then return x else empty;
 }

spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace c = elem c spaces

space :: Parser ()
space = 
    do {
        many (sat isSpace);
        return ();
    }

digits :: [Char]
digits = ['0'..'9']

isDigit :: Char -> Bool
isDigit c = elem c digits

digit :: Parser Char
digit = sat isDigit

lowers :: [Char]
lowers = ['a'..'z']

isLower :: Char -> Bool
isLower c = elem c lowers

lower :: Parser Char
lower = sat isLower

uppers :: [Char]
uppers = ['A'..'Z']

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
string (x:xs) = 
    do{
        char x;
        string xs;
        return (x:xs);
    }

anIdentifier :: Parser String
anIdentifier = 
    do {
        x <- letter;
        xs <- many alphanum;
        return (x:xs);
    }

aNaturalNumber :: Parser Int
aNaturalNumber =
    do{
        xs <- some digit;
        return (read xs);
    }

int :: Parser Int
int = 
    do {
        char '-';
        n <- aNaturalNumber;
        return (-n);
    }
    <|>
    aNaturalNumber;

token :: Parser a -> Parser a
token p = 
    do {
        space;
        v <- p;
        space;
        return v;
    }

identifier :: Parser String
identifier = token anIdentifier

naturalNumber :: Parser Int
naturalNumber = token aNaturalNumber

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

aexp :: Parser AExp
aexp = ap <|> sp <|> aTerm
  where
    ap = do
      a <- aTerm
      symbol "+"
      Add a <$> aexp
    sp = do
      a <- aTerm
      symbol "-"
      Sub a <$> aexp

aTerm :: Parser AExp
aTerm = mp <|> aFactor
  where
    mp = do
      a <- aFactor
      symbol "*"
      Mul a <$> aTerm

aFactor :: Parser AExp
aFactor = cp <|> ip <|> nested
  where
    cp = Constant <$> naturalNumber
    ip = AVariable <$> identifier
    nested = do
      symbol "("
      ep <- aexp
      symbol ")"
      return ep

bexp :: Parser BExp
bexp =
    do
        b <- bTerm
        symbol "or"
        Or b <$> bexp
    <|>
    do
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
    <|>
    do
        symbol "False"
        return (Boolean False)
    <|>
    do
        symbol "not"
        Not <$> bexp
    <|>
    do
        symbol "("
        b <- bexp
        symbol ")"
        return b

comparison :: Parser BExp
comparison = 
    do
        a1 <- aexp;
        symbol "<";
        a2 <- aexp;
        return (Comparison a1 a2 Lt)
    
    <|>
    do
        a1 <- aexp;
        symbol "<=";
        a2 <- aexp;
        return (Comparison a1 a2 Le)
    <|>
    do
        a1 <- aexp;
        symbol ">";
        a2 <- aexp;
        return (Comparison a1 a2 Gt)
    <|>
    do
        a1 <- aexp;
        symbol ">=";
        a2 <- aexp;
        return (Comparison a1 a2 Ge)
    <|>
    do
        a1 <- aexp;
        symbol "==";
        a2 <- aexp;
        return (Comparison a1 a2 Eq)
    <|>
    do
        a1 <- aexp;
        symbol "!=";
        a2 <- aexp;
        return (Comparison a1 a2 Neq)

command :: Parser Command
command =
    variableDeclaration
    <|>
    assignment
    <|>
    ifThenElse
    <|>
    while
    <|>
    skip

program :: Parser [Command]
program = 
    do many command;

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
        i <- identifier;
        symbol "=";
        r <- Assignment i <$> aexp;
        symbol ";";
        return r

skip :: Parser Command
skip = 
    do 
        symbol "skip";
        symbol ";";
        return Skip
    

ifThenElse :: Parser Command
ifThenElse =
    do {
        symbol "if";
        symbol "(";
        b <- bexp;
        symbol ")";
        symbol "{";
        thenProgram <- program;
        do{
            symbol "}";
            symbol "else";
            symbol "{";
            elseProgram <- program;
            symbol "}";
            return (IfThenElse b thenProgram elseProgram);
        }
        <|>
        do {
            symbol "endif";
            return (IfThenElse b thenProgram [Skip]);
        }
    }

while :: Parser Command
while = 
    do 
        symbol "while";
        symbol "(";
        b <- bexp;
        symbol ")";
        symbol "{";
        p <- program;
        symbol "}";
        return (While b p)
    