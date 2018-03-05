{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SimpleScript.Eval where

import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.List
import Data.Foldable
import Data.Functor
import Control.Monad.Loops
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import SimpleScript.AST

data Value
    = NullValue
    | BooleanValue Bool
    | NumericValue Double
    | StringValue String
    | ListValue (IORef [Value])
    | RecordValue (IORef (Map String Value))
    | FunctionValue ([Value] -> IO Value)

instance Show Value where
    show NullValue              = "null"
    show (NumericValue x)       = show x
    show (StringValue x)        = show x
    show (BooleanValue True)    = "true"
    show (BooleanValue False)   = "false"
    show (ListValue _)          = "<list>"
    show (RecordValue _)        = "<record>"
    show (FunctionValue _)      = "<function>"

data Env = Env
    { variables :: Map String Value
    , parent :: Maybe Env
    }
    deriving (Show)

newtype Action a = Action { unAction :: ReaderT (IORef Env) IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader (IORef Env)
             )

newEnv :: IO (IORef Env)
newEnv = newIORef $ Env Map.empty Nothing

runAction :: Action a -> IO a
runAction (Action a) = newEnv >>= runReaderT a

liftNumeric :: (Double -> Double) -> Value -> Action Value
liftNumeric f (NumericValue v) = pure $ NumericValue (f v)
liftNumeric _ v = error $ "Value " ++ show v ++ " is not numeric"

liftNumeric2 :: (a -> Value)
             -> (Double -> Double -> a)
             -> Value
             -> Value
             -> Action Value
liftNumeric2 toVal f (NumericValue a) (NumericValue b) = pure $ toVal (f a b)
liftNumeric2 _ _ v1 v2 = error $  "Values "
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

setVariable :: String -> Expression -> Action ()
setVariable name expr = do
    env <- ask
    val <- evaluate expr
    liftIO $ modifyIORef' env (onVariables (Map.insert name val))

showValue :: Value -> Action String
showValue (ListValue ref) = do
    vals <- liftIO $ readIORef ref
    reprs <- traverse showValue vals
    pure $ "[" <> intercalate "," reprs <> "]"
showValue (RecordValue ref) = do
    vals <- liftIO $ readIORef ref
    pairs <- Map.toAscList <$> traverse showValue vals
    let reprs = map (\(k,v) -> k <> "=" <> v) pairs
    pure $ "record {" <> intercalate "," reprs <> "}"
showValue v = pure $ show v

testExecute :: Block -> IO (Maybe String)
testExecute block = runAction (execute block >>= traverse showValue)

firstJust :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJust [] = pure Nothing
firstJust (x:xs) = x >>= \case
    val@Just{}  -> pure val
    Nothing     -> firstJust xs

-- TODO scoping
execute :: Block -> Action (Maybe Value)
execute (Block stmts) = firstJust . map exec $ stmts where
    booleanValue (BooleanValue True)    = True
    booleanValue (BooleanValue False)   = False
    booleanValue _                      = error "non-boolean value"
    exec (If expr block elseBlock) = evaluate expr >>= \case
        (BooleanValue True)     -> execute block
        (BooleanValue False)    -> maybe (pure Nothing) execute elseBlock
        _                       -> error "non-boolean condition for if"
    -- TODO early exit
    exec (While expr block) = asum <$> whileM (booleanValue <$> evaluate expr)
                                              (execute block)
    exec For{} = error "for loops are unimplemented"
    exec (BlockStatement block) = execute block
    exec (ExpressionStatement expr) = evaluate expr $> Nothing
    exec (Definition var (Just expr)) = setVariable var expr $> Nothing
    exec (Definition _ _) = pure Nothing
    exec (Assignment var [] expr) = setVariable var expr $> Nothing
    exec (Assignment var [key] expr) = do
        (RecordValue r) <- getVariable var
        val <- evaluate expr
        liftIO $ modifyIORef r (Map.insert key val)
        pure Nothing
    exec Assignment{} = error "deep record assignment not implemented"
    exec (Return val) = Just <$> evaluate val

makeRecord :: [(String, Expression)] -> Action Value
makeRecord pairExprs = do
    pairs <- traverse (\(k, v) -> (k,) <$> evaluate v) pairExprs
    RecordValue <$> liftIO (newIORef (Map.fromList pairs))

makeList :: [Expression] -> Action Value
makeList elemExprs = do
    elems <- traverse evaluate elemExprs
    ListValue <$> liftIO (newIORef elems)

makeFunction :: [String] -> Block -> Action Value
makeFunction _ _ = pure . FunctionValue . const $ pure NullValue

applyFunction :: Expression -> [Expression] -> Action Value
applyFunction f args = do
    args' <- traverse evaluate args
    (FunctionValue f') <- evaluate f
    liftIO $ f' args'

lookupRecord :: Expression -> String -> Action Value
lookupRecord expr key = do
    (RecordValue r) <- evaluate expr
    (Map.! key) <$> liftIO (readIORef r)

getVariable :: String -> Action Value
getVariable name = do
        envRef <- ask
        env <- liftIO $ readIORef envRef
        lookupVar name env
    where
    lookupVar var env = case (Map.lookup var (variables env), parent env) of
        (Just x, _)                 -> pure x
        (Nothing, Just p)           -> lookupVar var p
        _                           -> error $ "variable " ++ var ++ " not found"

evalNumeric :: (a -> Value)
            -> (Double -> Double -> a)
            -> Expression
            -> Expression
            -> Action Value
evalNumeric toVal op a b = do
    av <- evaluate a
    bv <- evaluate b
    liftNumeric2 toVal op av bv

evaluate :: Expression -> Action Value
evaluate NullLiteral            = pure NullValue
evaluate TrueLiteral            = pure $ BooleanValue True
evaluate FalseLiteral           = pure $ BooleanValue False
evaluate (NumericLiteral val)   = pure $ NumericValue val
evaluate (StringLiteral val)    = pure $ StringValue val
evaluate (Variable s)           = getVariable s
evaluate (Negate val)           = evaluate val >>= liftNumeric negate
evaluate (a :< b)               = evalNumeric BooleanValue (<) a b
evaluate (a :== b)              = evalNumeric BooleanValue (==) a b -- TODO strings
evaluate (a :> b)               = evalNumeric BooleanValue (>) a b
evaluate (a :+ b)               = evalNumeric NumericValue (+) a b
evaluate (a :- b)               = evalNumeric NumericValue (-) a b
evaluate (a :* b)               = evalNumeric NumericValue (*) a b
evaluate (a :/ b)               = evalNumeric NumericValue (/) a b
evaluate (r :. k)               = lookupRecord r k
evaluate (_ :.: _)              = error "bind is undefined"
evaluate (FunctionCall f args)  = applyFunction f args
evaluate (Function args body)   = makeFunction args body
evaluate (List elems)           = makeList elems
evaluate (Record pairs)         = makeRecord pairs
