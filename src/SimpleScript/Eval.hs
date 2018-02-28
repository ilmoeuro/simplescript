{-# LANGUAGE TupleSections #-}
module SimpleScript.Eval where

import Data.IORef
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

import SimpleScript.Types

(\\\) :: Eq a => [a] -> [a] -> [a]
a \\\ b = filter (not . (`elem` b)) a

allVars :: Expression -> [String]
allVars (Variable s)            = [s]
allVars (Negate expr)           = allVars expr
allVars (a :+ b)                = allVars a ++ allVars b
allVars (a :- b)                = allVars a ++ allVars b
allVars (a :* b)                = allVars a ++ allVars b
allVars (a :/ b)                = allVars a ++ allVars b
allVars (a :. b)                = allVars a ++ allVars b
allVars (a :.: b)               = allVars a ++ allVars b
allVars (Function args body)    = unboundVars body \\\ args
allVars (FunctionCall f args)   = allVars f ++ concatMap allVars args
allVars (List elems)            = concatMap allVars elems
allVars (Record pairs)          = concatMap (allVars . snd) pairs
allVars _                       = []

declaredVars :: Block -> [String]
declaredVars (Block stmts) = mapMaybe vars stmts where
    vars (Definition s _) = Just s
    vars _                = Nothing

unboundVars :: Block -> [String]
unboundVars block@(Block stmts) = nub (concatMap vars stmts) \\\ declared where
    declared = declaredVars block
    vars (If expr block1 (Just block2)) = allVars expr ++ unboundVars block1 ++ unboundVars block2
    vars (If expr block' Nothing)   = allVars expr ++ unboundVars block'
    vars (While expr block')        = allVars expr ++ unboundVars block'
    vars (For var expr block')      = (allVars expr ++ unboundVars block') \\\ [var]
    vars (BlockStatement block')    = unboundVars block'
    vars (ExpressionStatement expr) = allVars expr
    vars (Definition _ (Just expr)) = allVars expr
    vars (Definition _ Nothing)     = []
    vars (Assignment expr1 expr2)   = allVars expr1 ++ allVars expr2
    vars (Return expr)              = allVars expr

makeRecord :: IORef Env -> [(String, Expression)] -> IO Value
makeRecord env pairExprs = do
    pairs <- traverse (\(k, v) -> (k,) <$> eval env v) pairExprs
    RecordValue <$> newIORef (Map.fromList pairs)

makeList :: IORef Env -> [Expression] -> IO Value
makeList env elemExprs = do
    elems <- traverse (eval env) elemExprs
    ListValue <$> newIORef elems

makeFunction :: IORef Env -> [String] -> Block -> IO Value
makeFunction _ _ _ = pure . FunctionValue . const $ pure NullValue

applyFunction :: IORef Env -> Expression -> [Expression] -> IO Value
applyFunction env f args = do
    args' <- traverse (eval env) args
    (FunctionValue f') <- eval env f
    f' args'

eval :: IORef Env -> Expression -> IO Value
eval _ NullLiteral              = pure NullValue
eval _ (NumericLiteral val)     = pure $ NumericValue val
eval _ (StringLiteral val)      = pure $ StringValue val
eval e (Variable s)             = (Map.! s) . variables <$> readIORef e
eval e (Negate val)             = liftNumeric negate <$> eval e val
eval e (a :+ b)                 = liftNumeric2 (+) <$> eval e a <*> eval e b
eval e (a :- b)                 = liftNumeric2 (-) <$> eval e a <*> eval e b
eval e (a :* b)                 = liftNumeric2 (*) <$> eval e a <*> eval e b
eval e (a :/ b)                 = liftNumeric2 (/) <$> eval e a <*> eval e b
eval e (FunctionCall f args)    = applyFunction e f args
eval e (Function args body)     = makeFunction e args body
eval e (List elems)             = makeList e elems
eval e (Record pairs)           = makeRecord e pairs