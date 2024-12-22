{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude (($), Either(..), String, (++), Show, show, unwords, foldl, map, Bool(..), Maybe(..), null, (!!), any, (&&), (+), (-), unlines, concatMap, reverse, div, mod, (*), not, (==), head, Eq, length, (/=), return, (||), Int, otherwise, error, Integer, (<), (<=), (>), (>=))
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate)

type Err = Either String
type VarEnv = Map.Map String String
type FunEnv = Map.Map String String

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

builtinFunctionsTypes :: FunEnv
builtinFunctionsTypes = Map.fromList
  [ ("printInt", "void")
  , ("printString", "void")
  , ("error", "void")
  , ("readInt", "i32")
  , ("readString", "i8*")
  ]

generateProgramCode :: Program -> [String]
generateProgramCode (Program _ topDefs) =
  let funEnv = foldl addFunctionToEnv builtinFunctionsTypes topDefs
  in concatMap (generateFunction funEnv) topDefs

addFunctionToEnv :: FunEnv -> TopDef -> FunEnv
addFunctionToEnv env (FnDef pos returnType ident args _) = 
  Map.insert (getIdentName ident) (getType returnType) env

generateFunction :: FunEnv -> TopDef -> [String]
generateFunction funEnv (FnDef _ returnType ident args block) =
  let varEnv = Map.empty
      llvmReturnType = getType returnType
      functionName = getIdentName ident 
      (llvmArgs, newVarEnv) = foldl generateFunctionArg ([], varEnv) args
      header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"
      (blockCode, _) = generateBlockCode block newVarEnv funEnv
  in header : reverse ("}\n" : blockCode)

generateFunctionArg :: ([String], VarEnv) -> Arg -> ([String], VarEnv)
generateFunctionArg (args, env) (Arg _ argType ident) =
  let
    llvmType = getType argType
    argName = getIdentName ident
    newEnv = Map.insert argName llvmType env
  in (args ++ [llvmType ++ " %" ++ argName], newEnv)

getIdentName :: Ident -> String
getIdentName (Ident name) = name

getType :: Type -> String
getType (Int _) = "i32"
getType (Str _) = "i8*"
getType (Bool _) ="i1"
getType (Void _) = "void"
getType (Fun _ returnType paramTypes) = getType returnType

generateBlockCode :: Block -> VarEnv -> FunEnv -> ([String], VarEnv)
generateBlockCode (Block _ stmts) varEnv funEnv = 
  let (blockCode, _, _) = foldl processStmt ([], varEnv, funEnv) stmts
  in (blockCode, varEnv)

processStmt :: ([String], VarEnv, FunEnv) -> Stmt -> ([String], VarEnv, FunEnv)
processStmt (codeBuffer, varEnv, funEnv) (Decl _ varType items) =
  let declCode = concatMap (generateVarDecl varType) items
      newVarEnv = addItemsToEnv varType items varEnv
  in (declCode ++ codeBuffer, newVarEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) (BStmt _ (Block _ stmts)) = foldl processStmt (codeBuffer, varEnv, funEnv) stmts
processStmt (codeBuffer, varEnv, funEnv) (Ass _ ident expr) =
  let
    variableName = "%" ++ getIdentName ident
    exprCode = generateExprCode expr
    assignCode = variableName ++ " = " ++ exprCode
  in (assignCode : codeBuffer, varEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) (Incr _ ident) =
  let
    variableName = "%" ++ getIdentName ident
    incrCode = variableName ++ " = add i32 " ++ variableName ++ ", 1"
  in (incrCode : codeBuffer, varEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) (Decr _ ident) =
  let
    variableName = "%" ++ getIdentName ident
    decrCode = variableName ++ " = sub i32 " ++ variableName ++ ", 1"
  in (decrCode : codeBuffer, varEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) (Ret _ expr) =
  let
    exprCode = generateExprCode expr
    retCode = "  ret " ++ checkExprType expr varEnv funEnv ++ " " ++ exprCode
  in (retCode : codeBuffer, varEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) (VRet _) =
  ("  ret void" : codeBuffer, varEnv, funEnv)
processStmt (codeBuffer, varEnv, funEnv) _ = (codeBuffer, varEnv, funEnv)  -- TODO

addItemsToEnv :: Type -> [Item] -> VarEnv -> VarEnv
addItemsToEnv varType items varEnv = 
  foldl addItemToEnv varEnv items
  where
    addItemToEnv :: VarEnv -> Item -> VarEnv
    addItemToEnv env (NoInit _ ident) =
      Map.insert (getIdentName ident) (getType varType) env
    addItemToEnv env (Init _ ident _) =
      Map.insert (getIdentName ident) (getType varType) env

checkExprType :: Expr -> VarEnv -> FunEnv -> String
checkExprType (ELitInt {}) _ _ = "i32"
checkExprType (EString {}) _ _ = "i8*"
checkExprType (EVar pos ident) varEnv _ = 
  case Map.lookup (getIdentName ident) varEnv of
    Just varType -> varType
    Nothing -> error ("Variable " ++ getIdentName ident ++ " not found in the environment. " ++ show pos)
checkExprType (EApp pos ident _) _ funEnv = 
  case Map.lookup (getIdentName ident) funEnv of
    Just returnType -> returnType
    Nothing -> error ("Function " ++ getIdentName ident ++ " not found in the function environment. " ++ show pos)
checkExprType (ELitTrue _) _ _ = "i1"
checkExprType (ELitFalse _) _ _ = "i1"
checkExprType (Neg {}) _ _ = "i32"
checkExprType (Not {}) _ _ = "i1"
checkExprType (EMul {}) _ _ = "i32"
checkExprType (EAdd _ arg1 _ _) varEnv funEnv = checkExprType arg1 varEnv funEnv
checkExprType (ERel _ arg1 _ _ ) varEnv funEnv = checkExprType arg1 varEnv funEnv
checkExprType (EAnd {}) _ _ = "i1"
checkExprType (EOr {}) _ _ = "i1"

-- generateVarDecl :: Type -> Item -> [String]
-- generateVarDecl varType (NoInit _ ident) =
--   ["%" ++ getIdentName ident ++ " = alloca " ++ getType varType]
-- generateVarDecl varType (Init _ ident expr) =
--   let
--     variableName = "%" ++ getIdentName ident
--     allocaCode = variableName ++ " = alloca " ++ getType varType
--     exprCode = generateExprCode expr
--     storeCode = "store " ++ getType varType ++ " " ++ exprCode ++ ", " ++ getType varType ++ "* " ++ variableName
--   in [allocaCode, storeCode]

generateVarDecl :: Type -> Item -> [String]
generateVarDecl varType (NoInit _ ident) =
  let
    variableName = "%" ++ getIdentName ident
    llvmType = getType varType
    defaultValue = getDefaultForType varType
    code = [variableName ++ " = " ++ defaultValue]
    updatedEnv = Map.insert variableName llvmType
  in code

generateVarDecl varType (Init _ ident expr) =
  let
    variableName = "%" ++ getIdentName ident
    llvmType = getType varType
    exprCode = generateExprCode expr
    code = [variableName ++ " = " ++ exprCode]
    updatedEnv = Map.insert variableName llvmType
  in code

getDefaultForType :: Type -> String
getDefaultForType (Int _) = "0"
getDefaultForType (Str _) = ""
getDefaultForType (Bool _) = "false"
getDefaultForType _ = error "Unsupported type for default value"


generateExprCode :: Expr -> String
generateExprCode (ELitInt _ value) = show value
generateExprCode (EString _ value) = "c\"" ++ escapeString value ++ "\""
generateExprCode (EVar _ ident) = "%" ++ getIdentName ident
generateExprCode _ = ""       --TODO

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\n' = "\\0A"
    escapeChar c = [c]
    