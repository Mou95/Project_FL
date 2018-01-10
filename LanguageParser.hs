module LanguageParser where

import System.IO
import Control.Monad
import Control.Applicative
import Data.Char



-- parser type
newtype Parser a = P(String -> [(a, String)])

-- apply a parser to a given input
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- parser is also a functor
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P (\inp -> case parse p inp of
                      [] -> []
                      [(v, out)] -> [(f v, out)])

-- parser is an Applicative
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P(\inp -> [(x, inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = P (\inp -> case parse pf inp of
                         [] -> []
                         [(f, out)] -> parse (fmap f px) out)

-- parser is a Monad
instance Monad Parser where
  return = pure
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P(\inp -> case parse p inp of
                     [] -> []
                     [(v, out)] -> parse (f v) out)

-- parser is an alternetive -> used to apply the rules of the grammar and deal with the fact that the rule applied could be the wrong one automatically
instance Alternative Parser where
  -- empty :: Parser a
  empty = P(\inp -> [])

  -- <|> :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P(\inp -> case parse p1 inp of
                        [] -> parse p2 inp
                        [(v, out)] -> [(v, out)])
  -- some :: Parser a -> Parser [a]
  some p = pure (:) <*> p <*> many p

  --many :: Parser a -> Parser [a]
  many p = some p <|> pure []


-- parser that extract the first character of a string and if it doesn't fail return a list with a couple with that char and the rest of the string
-- e.g. parse item "ciao" -> [('c', "iao")]
-- parse item "" -> []
item :: Parser Char
item = P(\inp -> case inp of
                   [] -> []
                   (x:xs) -> [(x,xs)])

-- function that given a predicate return a parser if the predicate is true
-- e.g. parse (sat (=='c')) "ciao" -> [('c',"iao")]
-- parse (sat (=='c')) "salve" -> []
sat :: (Char -> Bool) -> Parser Char
sat predicate = do x <- item
                   if (predicate x)
                     then return x
                     else empty

-- check if the first character of a string is a digit
-- e.g. parse (digit) "1234" -> [('1',"234")]
--      parse (digit) "ciao" -> []
digit :: Parser Char
digit = sat isDigit

-- check if the first character of a string is a lower case
-- e.g. parse (lower) "ciao" -> [('c',"iao")]
--      parse (lower) "Ciao" -> []
lower :: Parser Char
lower = sat isLower

-- check if the first character of a string is a letter
-- e.g. parse (lower) "ciao" -> [('c',"iao")]
--      parse (lower) "1234" -> []
letter :: Parser Char
letter = sat isAlpha

-- check if the first character of a string is an alphanumeric character
-- e.g. parse (alphanum) "ciao" -> [('c',"iao")]
--      parse (alphanum) " ciao" -> []
alphanum :: Parser Char
alphanum = sat isAlphaNum

--check if a string start with a certain character
-- e.g. parse (char 'c') "ciao" -> [('c',"iao")]
--      parse (char 'z') "ciao" -> []
char :: Char -> Parser Char
char c = sat (== c)

--check if a string start with a certain substring
-- e.g. parse (string "ci") "ciao" -> [("ci","ao")]
--      parse (string "ma") "ciao" -> []
string :: String -> Parser String
string [] = return []
string (c:cs) = do char c
                   string cs
                   return (c:cs)



-- parse a natural number
-- e.g. parse nat "88" -> [(88,"")]
--      parse nat "-3" -> []
--      parse nat "ciao" -> []
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- remove one or more spaces before a string
-- e.g. parse space "    ciao" -> [((),"ciao")]
--      parse space "ciao" -> [((),"ciao")]
space :: Parser ()
space = do many (sat isSpace)
           return ()

-- parse an int number: first it tries to read a '-' sign(negative integer), if it fails it tries to read a natural number(positive integer)
-- e.g. parse int "88" -> [(88,"")]
--      parse int "-3" -> [(-3,"")]
--      parse int "ciao" -> []
int :: Parser Int
int = do char '-'
         n <- nat
         return (-1 * n)
         <|>
         nat

--given a parser, remove the spaces before and after the value
token :: Parser a -> Parser a
token parser = do space
                  value <- parser
                  space
                  return value

-- parse an integer removing spaces before and after
-- e.g. parse int "   88   " -> [(88,"")]
--      parse int "   -3   " -> [(-3,"")]
--      parse int "ciao" -> []
integer :: Parser Int
integer = token int

-- recognize a given string removing spaces before and after
-- e.g. parse (string "ci") "   ci   ao" -> [("ci","ao")]
--      parse (string "ma") "ciao" -> []
symbol :: String -> Parser String
symbol xs = token (string xs)


--------------- PARSER CORE LANGUAGE --------------
type Name = String

data Expr a = EVar Name
            | ENum Int
            | EConstr Int Int
            | EAp (Expr a) (Expr a)
            | ELet
                  IsRec
                  [Def a]
                  (Expr a)
            | ECase
                  (Expr a)
                  [Alter a]
            | ELam [a] (Expr a)
             deriving Show
--type definiton per programmi e supercombinator
type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

--type for let e letrec
type Def a = (a, Expr a)
-- type for case
type Alter a = (Int, [a], Expr a)

data IsRec = NonRecursive | Recursive
             deriving Show

kword :: [Name]
kword = ["let", "letrec", "in", "case", "of", "Pack"]

-- parser per progamma
parseProg :: Parser (Program Name)
parseProg = do p <- parseScDef
               do symbol ";"
                  ps <- parseProg
                  return (p:ps)
                  <|> return [p]

-- parser per supercombinator
parseScDef :: Parser (ScDefn Name)
parseScDef = do v <- parseVar
                pf <- many parseVar
                char '='
                body <- parseExpr
                return (v, pf, body)

-- parser per le variabili
parseVar :: Parser Name
parseVar = do v <- identifier
              return v

-- check if a string could be an identifier (such as in the programming languages)
-- e.g. parse identifier "c1Ao" -> [("c1Ao","")]
--      parse identifier "Ciao" -> [("Ciao","")]
--      parse identifier "2ciao" -> []
--      parse identifier "ci_ao" -> [("ci_ao","")]
ident :: Parser Name
ident = do x <- letter
           xs <- many varch
           return (x:xs)

-- check if a string start with an alphanum or a '_'
-- e.g. parse varch "2c_" -> [("2","c_")]
--      parse varch "<er3" -> []
varch :: Parser Char
varch = do x <- sat isAlphaNum
           return x
        <|>
        do y <- char '_'
           return y

-- check if a string could be an identifier (such as in the programming languages) removing spaces before and after
-- e.g. parse identifier "   c1Ao   " -> [("c1Ao","")]
--      parse identifier "2ciao" -> []
--      parse identifier "ci_ao" -> [("ciao","")]
identifier :: Parser Name
identifier = token ident

-- parser per qualunque tipo di Expr
parseExpr :: Parser (Expr Name)
parseExpr = do symbol "let"
               def <- some parseDef
               symbol "in"
               body <- parseExpr
               return (ELet NonRecursive def body)
            <|>
            do symbol "letrec"
               def <- some parseDef
               symbol "in"
               body <- parseExpr
               return (ELet Recursive def body)
            <|>
            do char '\\'
               var <- some parseVar
               symbol "."
               expr <- parseExpr
               return (ELam var expr)
             <|>
            do symbol "case"
               expr <- parseExpr
               symbol "of"
               alt <- parseAltn
               return (ECase expr alt)
            <|>
            do x <- parseAExpr
               return x

parseDef :: Parser (Def Name)
parseDef = do x <- parseVar
              symbol "="
              expr <- parseExpr
              do many (symbol ";")
                 return (x,expr)

parseAExpr :: Parser (Expr Name)
parseAExpr = do symbol "Pack"
                symbol "{"
                num1 <- integer
                symbol ","
                num2 <- integer
                symbol "}"
                return (EConstr num1 num2)
             <|>
             do symbol "("
                expr <- parseExpr
                symbol ")"
                return expr
             <|>
             do x <- identifier
                return (EVar x)
             <|>
             do num <- integer
                return (ENum num)

-- parser per la lista di alt, ritorna una lista di Alter
-- alt1;...;altn
parseAltn :: Parser [(Alter Name)]
parseAltn = do alt <- parseAlt
               do symbol ";"
                  altl <- parseAltn
                  return (alt:altl)
                  <|>
                  return [alt]

--parser per il singolo alt, ritorna una tripla con numero, variabili ed espressione
parseAlt :: Parser (Alter Name)
parseAlt = do symbol "<"
              num <- integer
              symbol ">"
              var <- many parseVar
              symbol "->"
              expr <- parseExpr
              return (num, var, expr)
