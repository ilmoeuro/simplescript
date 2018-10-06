{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module SimpleScript.CodeGen (codegen) where

import Data.Foldable
import Data.Traversable
import qualified SimpleScript.AST as A
import qualified SimpleScript.Bytecode as B
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.RWS
{-# ANN module "HLint: ignore Reduce duplication" #-}

type Identifiers = [String]
type CodeGen res = RWS () [B.Opcode] Identifiers res

next :: MonadState [a] m => m a
next = get >>= \(x:xs) -> put xs >> pure x

collect :: [A.Expression] -> CodeGen [B.Variable]
collect args = for args $ \arg -> do
    v <- B.Variable <$> next
    tell [B.CreateVariable v]
    eval v arg
    pure v

intrinsic :: B.Variable
          -> [A.Expression]
          -> B.Intrinsic
          -> CodeGen ()
intrinsic v args intr = do
    vars <- collect args
    tell [B.AssignIntrinsicResult v intr vars]
    for_ vars $ \var -> tell [B.DestroyVariable var]

binaryOp :: A.Expression -> Maybe (A.Expression, A.Expression, B.Intrinsic)
binaryOp (a A.:< b) = Just (a, b, B.ILessThan)
binaryOp (a A.:<= b) = Just (a, b, B.ILessThanEq)
binaryOp (a A.:== b) = Just (a, b, B.IEquals)
binaryOp (a A.:>= b) = Just (a, b, B.IMoreThanEq)
binaryOp (a A.:> b) = Just (a, b, B.IMoreThan)
binaryOp (a A.:+ b) = Just (a, b, B.IPlus)
binaryOp (a A.:- b) = Just (a, b, B.IMinus)
binaryOp (a A.:* b) = Just (a, b, B.IMultiply)
binaryOp (a A.:/ b) = Just (a, b, B.IDivide)
binaryOp _ = Nothing

eval :: B.Variable -> A.Expression -> CodeGen ()
eval v A.NullLiteral = tell [B.AssignConst v B.VNull]
eval v A.TrueLiteral = tell [B.AssignConst v $ B.VBool True]
eval v A.FalseLiteral = tell [B.AssignConst v $ B.VBool False]
eval v (A.StringLiteral s) = tell [B.AssignConst v $ B.VString s]
eval v (A.NumericLiteral n) = tell [B.AssignConst v $ B.VNumber n]
eval v (A.Variable v') = tell [B.AssignVar v (B.Variable v')]
eval v (A.Negate a) = intrinsic v [a] B.INegate
eval v (a A.:. b) = intrinsic v [a] (B.IRecordAccess b)
eval v (a A.:.: b) = intrinsic v [a] (B.IBind b)
eval v (binaryOp -> Just (a,b,i)) = intrinsic v [a,b] i
eval v (A.FunctionCall f args) = do
    funcVar <- B.Variable <$> next
    tell [B.CreateVariable funcVar]
    eval funcVar f
    argVars <- collect args
    tell [B.AssignResult v funcVar argVars]
    tell [B.DestroyVariable funcVar]
    for_ argVars $ \var -> tell [B.DestroyVariable var]
eval _ (A.Function _ _) = undefined
eval v (A.List elems) = intrinsic v elems B.IMakeList
eval v (A.Record (unzip -> (keys, vals))) = intrinsic v vals (B.IMakeRecord keys)

desugarFor :: String -> A.Expression -> A.Block -> CodeGen A.Statement
desugarFor iterator expr block = do
    iteratorVar <- next
    pure $ (A.BlockStatement . A.Block)
        [ A.Definition iteratorVar (Just (A.FunctionCall (expr A.:.: "iterator") []))
        , A.While (A.FunctionCall (A.Variable iteratorVar A.:.: "hasNext") []) $ A.Block
            [ A.Definition iterator
                           (Just (A.FunctionCall (A.Variable iteratorVar A.:.: "next") []))
            , A.BlockStatement block
            ]
        ]

exec :: A.Statement -> CodeGen ()
exec (A.If expr body Nothing) = do
    afterBody <- B.Label <$> next
    var <- B.Variable <$> next
    tell [B.CreateVariable var]
    intrinsic var [expr] B.INot
    tell [B.IfGoto var afterBody]
    tell [B.DestroyVariable var]
    exec (A.BlockStatement body)
    tell [B.DefineLabel afterBody]
exec (A.If expr ifBody (Just elseBody)) = do
    afterIf <- B.Label <$> next
    afterElse <- B.Label <$> next
    var <- B.Variable <$> next
    tell [B.CreateVariable var]
    intrinsic var [expr] B.INot
    tell [B.IfGoto var afterIf]
    tell [B.DestroyVariable var]
    exec (A.BlockStatement ifBody)
    tell [B.Goto afterElse, B.DefineLabel afterIf]
    exec (A.BlockStatement elseBody)
    tell [B.DefineLabel afterElse]
exec (A.While expr body) = do
    beforeWhile <- B.Label <$> next
    afterBody <- B.Label <$> next
    tell [B.DefineLabel beforeWhile]
    var <- B.Variable <$> next
    tell [B.CreateVariable var]
    intrinsic var [expr] B.INot
    tell [B.IfGoto var afterBody]
    tell [B.DestroyVariable var]
    exec (A.BlockStatement body)
    tell [B.Goto beforeWhile]
    tell [B.DefineLabel afterBody]
exec (A.For iterator expr block) = desugarFor iterator expr block >>= exec
exec (A.BlockStatement (A.Block stmts)) = do
    let varNames = [var | (A.Definition var _) <- stmts]
    for_ stmts exec
    for_ varNames $ \varName -> tell [B.DestroyVariable (B.Variable varName)]
exec (A.ExpressionStatement expr) = do
    tempVar <- B.Variable <$> next
    tell [B.CreateVariable tempVar]
    eval tempVar expr
    tell [B.DestroyVariable tempVar]
exec (A.Definition varName Nothing) = do
    let var = B.Variable varName
    tell [B.CreateVariable var]
    tell [B.AssignConst var B.VNull]
exec (A.Definition varName (Just initialExpr)) = do
    let var = B.Variable varName
    tell [B.CreateVariable var]
    eval var initialExpr
exec (A.Assignment varName [] expr) =
    eval (B.Variable varName) expr
exec (A.Return expr) = do
    tempVar <- B.Variable <$> next
    tell [B.CreateVariable tempVar]
    eval tempVar expr
    tell [B.Return tempVar]
exec (A.Export str expr) = do
    tempVar <- B.Variable <$> next
    tell [B.CreateVariable tempVar]
    intrinsic tempVar [expr] (B.IExport str)
    tell [B.DestroyVariable tempVar]
exec (A.Import modName members) = do
    let modVar = B.Variable modName
    tell [B.CreateVariable modVar]
    intrinsic modVar [] (B.IImport modName)
    for_ members $ \member -> do
        let var = B.Variable member
        tell [B.CreateVariable var]
        tell [B.AssignIntrinsicResult var (B.IRecordAccess member) [modVar]]
exec (A.LineNumber ln) = tell [B.LineNumber ln]

codegen :: [A.Statement] -> [B.Opcode]
codegen stmts = snd $ execRWS (for_ stmts exec) () ["$" <> show i | i <- [1::Int ..]]
    
{-
exec For 
exec BlockStatement 
exec ExpressionStatement 
exec Definition 
exec Assignment 
exec Return 
exec Export 
exec Import 
exec LineNumber 
-}