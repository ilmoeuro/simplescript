{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Strict #-}
module SimpleScript.Bytecode
    ( Variable (..)
    , Label (..)
    , Value (..)
    , Intrinsic (..)
    , Opcode (..)
    , toAssembly
    ) where

import Data.List

newtype Variable = Variable { varName :: String }
    deriving (Show)

newtype Label = Label { labelName :: String }
    deriving (Show)

data Value
    = VNull
    | VBool Bool
    | VNumber Double
    | VString String
    deriving (Show)

data Intrinsic
    = INot
    | INegate
    | ILessThan
    | ILessThanEq
    | IEquals
    | IMoreThanEq
    | IMoreThan
    | IPlus
    | IMinus
    | IMultiply
    | IDivide
    | IRecordAccess String
    | IBind String
    | IMakeList
    | IMakeRecord [String]
    | IMakeFunction [Variable] [Opcode]
    | IRecordAssign String
    | IImport String
    | IExport String
    deriving (Show)

data Opcode
    = AssignConst Variable Value
    | AssignVar Variable Variable
    | AssignArg Variable Int
    | AssignClosure Variable Int
    | AssignResult Variable Variable [Variable]
    | AssignIntrinsicResult Variable Intrinsic [Variable]
    | CreateVariable Variable
    | DestroyVariable Variable
    | DefineLabel Label
    | Goto Label
    | IfGoto Variable Label
    | Return Variable
    | LineNumber Int
    deriving (Show)

toAssembly :: [Opcode] -> String
toAssembly = intercalate "\n" . map show