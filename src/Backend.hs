{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude (($), Either(..), String, (++), Show, show, unwords, foldl, map, Bool(..), Maybe(..), null, (!!), any, (&&), (+), (-), unlines, concatMap, reverse, div, mod, (*), not, (==), head, Eq, length, (/=), return, (||), Int, otherwise, error, Integer, (<), (<=), (>), (>=))
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate)

generateLLVM :: Program -> String
generateLLVM program =
  let codeBuffer = generatePredefinedFunctions ++ generateProgramCode program
  in unlines codeBuffer

generatePredefinedFunctions :: [String]
generatePredefinedFunctions =
  [ "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()\n"
  ]

generateProgramCode :: Program -> [String]
generateProgramCode (Program _ topDefs) =
  concatMap generateFunction topDefs

generateFunction :: TopDef -> [String]
generateFunction (FnDef _ returnType ident args block) =
  let llvmReturnType = getType returnType
      functionName = getIdentName ident
      llvmArgs = map generateFunctionArg args
      header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"
      blockCode = generateBlockCode block []
  in header : reverse ("}\n" : blockCode)

generateFunctionArg :: Arg -> String
generateFunctionArg (Arg _ argType ident) =
  let llvmType = getType argType
      argName = getIdentName ident
  in llvmType ++ " %" ++ argName

getIdentName :: Ident -> String
getIdentName (Ident name) = name

getType :: Type -> String
getType (Int _) = "i32"
getType (Str _) = "i8*"
getType (Bool _) ="i1"
getType (Void _) = "void"
getType (Fun _ returnType paramTypes) = getType returnType

generateBlockCode :: Block -> [String] -> [String]
generateBlockCode (Block _ stmts) codeBuffer = foldl processStmt codeBuffer stmts

processStmt :: [String] -> Stmt -> [String]
processStmt codeBuffer (Decl _ varType items) =
  let declCode = concatMap (generateVarDecl varType) items
  in declCode ++ codeBuffer
processStmt codeBuffer (BStmt _ (Block _ stmts)) = foldl processStmt codeBuffer stmts
processStmt codeBuffer _ = codeBuffer  -- TODO

generateVarDecl :: Type -> Item -> [String]
generateVarDecl varType (NoInit _ ident) =
  ["%" ++ getIdentName ident ++ " = alloca " ++ getType varType]
generateVarDecl varType (Init _ ident expr) =
  let
    variableName = "%" ++ getIdentName ident
    allocaCode = variableName ++ " = alloca " ++ getType varType
    exprCode = generateExprCode expr
    storeCode = "store " ++ getType varType ++ " " ++ exprCode ++ ", " ++ getType varType ++ "* " ++ variableName
  in [allocaCode, storeCode]


generateExprCode :: Expr -> String
generateExprCode (ELitInt _ value) = show value
generateExprCode (EString _ value) = "c\"" ++ escapeString value ++ "\""
generateExprCode (EVar _ ident) = "%" ++ getIdentName ident
generateExprCode _ = ""     -- generate expression code

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\n' = "\\0A"
    escapeChar c = [c]
    