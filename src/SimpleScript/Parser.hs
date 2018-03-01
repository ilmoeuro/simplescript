module SimpleScript.Parser
    ( expression
    , statement
    , block
    , parse
    , (#)
    ) where

import Data.Void
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
                then fail $ "keyword "
                          ++ show x
                          ++ " cannot be an identifier"
                else return x

block :: Parser Block
block = Block <$> braces (many statement)

accessorOperators :: [[Operator Parser Expression]]
accessorOperators =
    [ [ InfixL ((:.) <$ symbol ".")
      , InfixL ((:.:) <$ symbol ":") ] ]

operators :: [[Operator Parser Expression]]
operators =
    accessorOperators ++
    [ [ Prefix (Negate <$ symbol "-")
      , Prefix (id <$ symbol "+") ]
    , [ InfixR ((:*) <$ symbol "*")
      , InfixR ((:/) <$ symbol "/") ]
    , [ InfixR ((:+) <$ symbol "+")
      , InfixR ((:-) <$ symbol "-") ] ]

expression :: Parser Expression
expression = makeExprParser term operators <?> "expression"

accessor :: Parser Expression
accessor =
    makeExprParser
        (parens expression <|> Variable <$> identifier)
        accessorOperators

recordEntry :: Parser (String, Expression)
recordEntry = (,) <$> identifier <* symbol "=" <*> expression

term :: Parser Expression
term = choice
    [ try (Function <$> parens (identifier `sepBy` comma) <*> block)
    , List <$> brackets (expression `sepBy` comma)
    , Record <$> (rword "record" *> braces (recordEntry `sepBy` comma))
    , parens expression
    , StringLiteral <$> (char '"' *> many (noneOf "\"") <* char '"' <* sc)
    , try (NumericLiteral <$> lexeme L.float)
    , NumericLiteral . fromInteger <$> lexeme L.decimal
    , try (FunctionCall <$> accessor <*> parens (expression `sepBy` comma))
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
        <$> expression
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