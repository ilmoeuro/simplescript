{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module SimpleScript.Eval where

import Data.IORef
import Data.Maybe
import Data.List
import Data.Foldable
import Data.Function
import Control.Monad
import Control.Monad.Loops
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SimpleScript.Types

type Action = IO

newEnv :: IO (IORef Env)
newEnv = newIORef $ Env Map.empty Nothing

liftNumeric :: (Double -> Double) -> Value -> Action Value
liftNumeric f (NumericValue v) = pure $ NumericValue (f v)
liftNumeric _ v = error $ "Value " ++ show v ++ " is not numeric"

liftNumeric2 :: (Double -> Double -> Double) -> Value -> Value -> Action Value
liftNumeric2 f (NumericValue a) (NumericValue b) = pure $ NumericValue (f a b)
liftNumeric2 _ v1 v2 = error $ "Values "
                               ++ show v1
                               ++ " and "
                               ++ show v2
                               ++ " are not numeric"

(\\\) :: Eq a => [a] -> [a] -> [a]
a \\\ b = filter (not . (`elem` b)) a

allVars :: Expression -> [String]
allVars (Variable s)            = [s]
allVars (Negate expr)           = allVars expr
allVars (a :+ b)                = allVars a ++ allVars b
allVars (a :- b)                = allVars a ++ allVars b
allVars (a :* b)                = allVars a ++ allVars b
allVars (a :/ b)                = allVars a ++ allVars b
allVars (a :. _)                = allVars a
allVars (a :.: _)               = allVars a
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
    vars (Assignment var _ expr)    = var : allVars expr
    vars (Return expr)              = allVars expr

onVariables :: (Map String Value -> Map String Value) -> Env -> Env
onVariables f e@Env{variables} = e { variables = f variables }

setVariable :: IORef Env -> String -> Expression -> Action ()
setVariable env name expr = do
    val <- eval env expr
    modifyIORef' env (onVariables (Map.insert name val))

execute :: IORef Env -> Block -> Action ()
execute env (Block stmts) = traverse_ exec stmts where
    booleanValue (BooleanValue True)    = True
    booleanValue (BooleanValue False)   = False
    booleanValue _                      = error "non-boolean value"
    exec (If expr block elseBlock) = eval env expr >>= \case
        (BooleanValue True)     -> execute env block
        (BooleanValue False)    -> maybe (pure ()) (execute env) elseBlock
        _                       -> error "non-boolean condition for if"
    exec (While expr block) = whileM_ (booleanValue <$> eval env expr)
                                      (execute env block)
    exec For{} = error "for loops are unimplemented"
    exec (BlockStatement block) = execute env block
    exec (ExpressionStatement expr) = void $ eval env expr
    exec (Definition var (Just expr)) = setVariable env var expr
    exec (Definition _ _) = pure ()
    exec (Assignment var [] expr) = setVariable env var expr
    exec (Assignment var [key] expr) = do
        (RecordValue r) <- getVariable env var
        val <- eval env expr
        modifyIORef r (Map.insert key val)
    exec Assignment{} = error "deep record assignment not implemented"
    exec Return{} = error "return not implemented"

makeRecord :: IORef Env -> [(String, Expression)] -> Action Value
makeRecord env pairExprs = do
    pairs <- traverse (\(k, v) -> (k,) <$> eval env v) pairExprs
    RecordValue <$> newIORef (Map.fromList pairs)

makeList :: IORef Env -> [Expression] -> Action Value
makeList env elemExprs = do
    elems <- traverse (eval env) elemExprs
    ListValue <$> newIORef elems

makeFunction :: IORef Env -> [String] -> Block -> Action Value
makeFunction _ _ _ = pure . FunctionValue . const $ pure NullValue

applyFunction :: IORef Env -> Expression -> [Expression] -> Action Value
applyFunction env f args = do
    args' <- traverse (eval env) args
    (FunctionValue f') <- eval env f
    f' args'

lookupRecord :: IORef Env -> Expression -> String -> Action Value
lookupRecord env expr key = do
    (RecordValue r) <- eval env expr
    (Map.! key) <$> readIORef r

getVariable :: IORef Env -> String -> Action Value
getVariable envRef name = readIORef envRef >>= lookupVar name where
    lookupVar var env = case (Map.lookup var (variables env), parent env) of
        (Just x, _)                 -> pure x
        (Nothing, Just p)           -> lookupVar var p
        _                           -> error $ "variable " ++ var ++ " not found"

evalNumeric :: (Double -> Double -> Double)
            -> IORef Env
            -> Expression 
            -> Expression
            -> Action Value
evalNumeric op env a b = do
    av <- eval env a
    bv <- eval env b
    liftNumeric2 op av bv

eval :: IORef Env -> Expression -> Action Value
eval _ NullLiteral              = pure NullValue
eval _ TrueLiteral              = pure $ BooleanValue True
eval _ FalseLiteral             = pure $ BooleanValue False
eval _ (NumericLiteral val)     = pure $ NumericValue val
eval _ (StringLiteral val)      = pure $ StringValue val
eval e (Variable s)             = getVariable e s
eval e (Negate val)             = eval e val >>= liftNumeric negate
eval e (a :+ b)                 = evalNumeric (+) e a b
eval e (a :- b)                 = evalNumeric (-) e a b
eval e (a :* b)                 = evalNumeric (*) e a b
eval e (a :/ b)                 = evalNumeric (/) e a b
eval e (FunctionCall f args)    = applyFunction e f args
eval e (Function args body)     = makeFunction e args body
eval e (List elems)             = makeList e elems
eval e (Record pairs)           = makeRecord e pairs