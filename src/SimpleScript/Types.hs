module SimpleScript.Types
    ( Expression (..)
    , Block (..)
    , Statement (..)
    , Value (..)
    , Env (..)
    , liftNumeric
    , liftNumeric2
    ) where

import Data.Map.Strict (Map)
import Data.IORef

data Expression
    = NullLiteral
    | TrueLiteral
    | FalseLiteral
    | StringLiteral String
    | NumericLiteral Double
    | Variable String
    | Negate Expression
    | Expression :+ Expression
    | Expression :- Expression
    | Expression :* Expression
    | Expression :/ Expression
    | Expression :. Expression
    | Expression :.: Expression
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
    | Assignment Expression Expression
    | Return Expression
    deriving (Show)

data Value
    = NullValue
    | BooleanValue Bool
    | NumericValue Double
    | StringValue String
    | ListValue (IORef [Value])
    | RecordValue (IORef (Map String Value))
    | FunctionValue ([Value] -> IO Value)

instance Show Value where
    show NullValue          = "null"
    show (NumericValue x)   = show x
    show (StringValue x)    = show x
    show (ListValue _)      = "<list>"
    show (RecordValue _)    = "<record>"
    show (FunctionValue _)  = "<function>"

data Env = Env
    { variables :: Map String Value
    , parent :: Maybe Env
    }

liftNumeric :: (Double -> Double) -> Value -> Value
liftNumeric f (NumericValue v) = NumericValue (f v)
liftNumeric _ v = error $ "Value " ++ show v ++ " is not numeric"

liftNumeric2 :: (Double -> Double -> Double) -> Value -> Value -> Value
liftNumeric2 f (NumericValue a) (NumericValue b) = NumericValue (f a b)
liftNumeric2 _ v1 v2 = error $ "Values "
                               ++ show v1
                               ++ " and "
                               ++ show v2
                               ++ " are not numeric"