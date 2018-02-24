module Lib
    ( someFunc
    ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

data Identifier
    = MkIdentifier String
    deriving (Show, Eq)

data Literal
    = NumericLiteral Integer
    | StringLiteral String
    deriving (Show, Eq)

data Expression
    = ExTerm Term
    | ExSum Term Term
    | ExDiff Term Term
    deriving (Show, Eq)

data Term
    = TeFactor Factor
    | TeProduct Factor Factor
    | TeQuotient Factor Factor
    deriving (Show, Eq)

data Factor
    = FaAtom Atom
    | FaNegate Atom
    deriving (Show, Eq)

data Atom
    = AtFunctionCall FunctionCall
    | AtFunction Function
    | AtRecordAccess RecordAccess
    | AtMethodCall MethodCall
    | AtVariable Variable
    | AtList List 
    | AtRecord Record 
    | AtLiteral Literal 
    | AtParenthesizedExpression Expression
    deriving (Show, Eq)

data FunctionCall
    = MkFunctionCall Atom [Expression]
    deriving (Show, Eq)

data Function
    = MkFunction [Identifier] Block
    deriving (Show, Eq)

data RecordAccess
    = MkRecordAccess Atom Identifier
    deriving (Show, Eq)

data MethodCall
    = MkMethodCall Atom [Expression]
    deriving (Show, Eq)

data Variable
    = MkVariable Identifier
    deriving (Show, Eq)

data List
    = MkList [Expression]
    deriving (Show, Eq)

data RecordPair
    = MkRecordPair Identifier Expression
    deriving (Show, Eq)

data Record
    = MkRecord [RecordPair]
    deriving (Show, Eq)

data Block = MkBlock
    deriving (Show, Eq)

sc :: Parser ()
sc = L.space space1 lineComment blockComment
    where
        lineComment = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- rword :: String -> Parser ()
-- rword w = lexeme (string w *> notFollowedBy alphaNumChar)

rws :: [String]
rws = ["if", "else", "while", "for", "let"]

comma :: Parser String
comma = symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Identifier
identifier = MkIdentifier <$> (lexeme . try) (p >>= check) where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword "
                          ++ show x
                          ++ " cannot be an identifier"
                else return x

literal :: Parser Literal
literal = NumericLiteral <$> lexeme L.decimal

block :: Parser Block
block = MkBlock <$ symbol "{}"

expression :: Parser Expression
expression =
        ExTerm <$> term
    <|> ExSum <$> term <* symbol "+" <*> term
    <|> ExDiff <$> term <* symbol "-" <*> term

term :: Parser Term
term =
        TeFactor <$> factor
    <|> TeProduct <$> factor <* symbol "*" <*> factor
    <|> TeQuotient <$> factor <* symbol "/" <*> factor

factor :: Parser Factor
factor =
        FaAtom <$> atom
    <|> FaNegate <$> (symbol "-" *> atom)

atom :: Parser Atom
atom =
        AtFunctionCall <$> functionCall
    <|> AtFunction <$> function
    <|> AtRecordAccess <$> recordAccess
    <|> AtMethodCall <$> methodCall
    <|> AtVariable <$> variable
    <|> AtList <$> list
    <|> AtRecord <$> record
    <|> AtLiteral <$> literal
    <|> AtParenthesizedExpression <$> parens expression

functionCall :: Parser FunctionCall
functionCall = MkFunctionCall
    <$> atom
    <*> parens (expression `sepBy` comma)

function :: Parser Function
function = MkFunction
    <$> parens (identifier `sepBy` comma)
    <*> block

recordAccess :: Parser RecordAccess
recordAccess = MkRecordAccess
    <$> atom
    <*  symbol "."
    <*> identifier

methodCall :: Parser MethodCall
methodCall = MkMethodCall
    <$> atom
    <*> parens (expression `sepBy` comma)

variable :: Parser Variable
variable = MkVariable <$> identifier

list :: Parser List
list = MkList <$>
            between (symbol "[") (symbol "]")
                (expression `sepBy` comma)

recordPair :: Parser RecordPair
recordPair = MkRecordPair <$> identifier <* symbol "=" <*> expression

record :: Parser Record
record = MkRecord <$>
            between (symbol "{") (symbol "}")
                (recordPair `sepBy` comma)

someFunc :: IO ()
someFunc = interact $ show . parse expression "<input>"