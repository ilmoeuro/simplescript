module SimpleScript.Parser
    ( expression
    , statement
    , block
    , parse
    , (#)
    ) where

import Data.Void
import Data.Functor
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Show.Pretty (pPrint)
import qualified Text.Megaparsec.Char.Lexer as L

import SimpleScript.Types

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = [ "if"
      , "else"
      , "while"
      , "for"
      , "in"
      , "let"
      , "record"
      , "null"
      , "true"
      , "false"]

comma :: Parser String
comma = symbol ","

semi :: Parser String
semi = symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $  "keyword "
                          ++ show x
                          ++ " cannot be an identifier"
                else return x

block :: Parser Block
block = Block <$> braces (many statement)

recordAccess :: Parser (Expression -> Expression)
recordAccess = flip (:.) <$> try (symbol "." *> identifier)

bind :: Parser (Expression -> Expression)
bind = flip (:.:) <$> try (symbol ":" *> identifier)

functionCall :: Parser (Expression -> Expression)
functionCall = flip FunctionCall <$> try (parens (expression `sepBy` comma))

manyPostfixOp :: Parser (a -> a) -> Parser (a -> a)
manyPostfixOp singleOp = foldr1 (flip (.)) <$> some singleOp

operators :: [[Operator Parser Expression]]
operators =
    [ [ Postfix (manyPostfixOp recordAccess) ]
    , [ Postfix (manyPostfixOp bind) ]
    , [ Postfix (manyPostfixOp functionCall) ]
    , [ Prefix (Negate <$ symbol "-")
      , Prefix (id <$ symbol "+") ]
    , [ InfixR ((:*) <$ symbol "*")
      , InfixR ((:/) <$ symbol "/") ]
    , [ InfixR ((:+) <$ symbol "+")
      , InfixR ((:-) <$ symbol "-") ] ]

expression :: Parser Expression
expression = makeExprParser term operators <?> "expression"

recordEntry :: Parser (String, Expression)
recordEntry = (,) <$> identifier <* symbol "=" <*> expression

quotedString :: Parser String
quotedString
        =   char '"'
        *>  (many . choice)
            [ noneOf "\\\""
            , string "\\\"" $> '"'
            , string "\\\\" $> '\\' ]
        <*  char '"'

term :: Parser Expression
term = choice
    [ try $ Function <$> parens (identifier `sepBy` comma) <*> block
    , List <$> brackets (expression `sepBy` comma)
    , Record <$ rword "record" <*> braces (recordEntry `sepBy` comma)
    , parens expression
    , StringLiteral <$> quotedString <* sc
    , try $ NumericLiteral <$> lexeme L.float
    , NumericLiteral . fromInteger <$> lexeme L.decimal
    , try $ NullLiteral <$ rword "null"
    , try $ TrueLiteral <$ rword "true"
    , try $ FalseLiteral <$ rword "false"
    , Variable <$> identifier
    ]
    <?> "term"

statement :: Parser Statement
statement = choice
    [ If
        <$  rword "if"
        <*> parens expression
        <*> block
        <*> optional (rword "else" *> block)
    , While
        <$  rword "while"
        <*> parens expression
        <*> block
    , For
        <$  rword "for"
        <*  symbol "("
        <*> identifier
        <*  rword "in"
        <*> expression
        <*  symbol ")"
        <*> block
    , Return
        <$  rword "return"
        <*> expression
        <*  semi
    , BlockStatement <$> block
    , try (ExpressionStatement <$> expression <* semi)
    , Definition
        <$  rword "let"
        <*> identifier
        <*> optional (symbol "=" *> expression)
        <*  semi
    , Assignment
        <$> identifier
        <*> (identifier `sepBy` symbol ".")
        <*  symbol "="
        <*> expression
        <*  semi
    ]
    <?> "statement"

parse :: Parser a -> String -> a
parse parser input =
    either
        (error . parseErrorPretty' input)
        id
        (MP.parse (sc *> parser <* eof) "" input)

(#) :: Show a => Parser a -> String -> IO ()
(#) e i = either
            (putStrLn . parseErrorPretty' i)
            pPrint
            (MP.parse (sc *> e <* eof) "" i)