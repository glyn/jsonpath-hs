module Data.JSONPath.TypeChecker
  ( FunctionExpr (..),
    FunctionArgument (..),
    FunctionName (..),
    checkTypes,
  )
where

import Data.JSONPath.Types
import Data.Text

data FunctionExpr
  = FunctionExpr FunctionName [FunctionArgument]
  deriving (Show, Eq)

type FunctionName = Text

data FunctionArgument
  = ArgLiteral Literal
  | ArgFilterQuery FilterQuery
  | ArgLogicalExpr FilterExpr
  | ArgFunctionExpr FunctionExpr
  deriving (Show, Eq)

checkTypes :: FunctionExpr -> TypedFunctionExpr -- TODO: needs monad to capture type errors
checkTypes = undefined