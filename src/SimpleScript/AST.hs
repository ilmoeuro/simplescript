module SimpleScript.AST
    ( Expression (..)
    , Block (..)
    , Statement (..)
    ) where

data Expression
    = NullLiteral
    | TrueLiteral
    | FalseLiteral
    | StringLiteral String
    | NumericLiteral Double
    | Variable String
    | Negate Expression
    | Expression :< Expression
    | Expression :<= Expression
    | Expression :== Expression
    | Expression :>= Expression
    | Expression :> Expression
    | Expression :+ Expression
    | Expression :- Expression
    | Expression :* Expression
    | Expression :/ Expression
    | Expression :. String
    | Expression :.: String
    | FunctionCall Expression [Expression]
    | Function [String] Block
    | List [Expression]
    | Record [(String, Expression)]
    deriving (Show)

newtype Block = Block [Statement]
    deriving (Show)

data Statement
    = If Expression Block (Maybe Block)
    | While Expression Block
    | For String Expression Block
    | BlockStatement Block
    | ExpressionStatement Expression
    | Definition String (Maybe Expression)
    | Assignment String [String] Expression
    | Return Expression
    deriving (Show)