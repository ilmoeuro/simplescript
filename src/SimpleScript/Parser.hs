module SimpleScript.Parser
    ( expression
    , statement
    , sourceFile
    , block
    , parse
    , (#)
    ) where

import Data.Void
import Data.Functor
import Control.Monad
import Text.Megaparsec hiding (parse)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Text.Show.Pretty (pPrint)
import qualified Text.Megaparsec.Char.Lexer as L

import SimpleScript.AST

type Parser = Parsec Void String

normalizeMaybe :: Maybe [a] -> [a]
normalizeMaybe (Just x) = x
normalizeMaybe Nothing = []

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
      , "false"
      , "export"
      , "import"]

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

statements :: Parser [Statement]
statements = join <$> many numberedStatement

numberedStatement :: Parser [Statement]
numberedStatement = sequence
    [ LineNumber . unPos . sourceLine <$> getPosition
    , statement
    ]

block :: Parser Block
block = Block <$> braces statements

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
    [ [ Postfix . manyPostfixOp $ recordAccess <|> bind <|> functionCall ]
    , [ Prefix $ Negate <$ symbol "-"
      , Prefix $ id <$ symbol "+" ]
    , [ InfixR $ (:*) <$ symbol "*"
      , InfixR $ (:/) <$ symbol "/" ]
    , [ InfixR $ (:+) <$ symbol "+"
      , InfixR $ (:-) <$ symbol "-" ]
    , [ InfixR . try $ (:<=) <$ symbol "<="
      , InfixR . try $ (:==) <$ symbol "=="
      , InfixR . try $ (:>=) <$ symbol ">="
      , InfixR $ (:<) <$ symbol "<"
      , InfixR $ (:>) <$ symbol ">" ] ]

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
    , try $ Record <$ rword "record" <*> braces (recordEntry `sepBy` comma)
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

blockOrIf :: Parser Block
blockOrIf =   block
          <|> (Block . (:[])) <$> ifStatement

ifStatement :: Parser Statement
ifStatement =
    If
        <$  rword "if"
        <*> parens expression
        <*> block
        <*> optional (rword "else" *> blockOrIf)

statement :: Parser Statement
statement = (choice . map try)
    [ ifStatement
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
    , BlockStatement <$> block
    , ExpressionStatement <$> expression <* semi
    , Definition
        <$  rword "let"
        <*> identifier
        <*> optional (symbol "=" *> expression)
        <*  semi
    , Assignment
        <$> identifier
        <*> many (symbol "." *> identifier)
        <*  symbol "="
        <*> expression
        <*  semi
    , Return
        <$  rword "return"
        <*> expression
        <*  semi
    , Export
        <$  rword "export"
        <*> identifier
        <*  symbol "="
        <*> expression
        <*  semi
    , Import
        <$  rword "import"
        <*> identifier
        <*> (normalizeMaybe <$> optional (braces (identifier `sepEndBy` comma)))
    ]
    <?> "statement"

sourceFile :: Parser [Statement]
sourceFile = statements

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