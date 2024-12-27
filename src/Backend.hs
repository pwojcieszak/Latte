{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Functor ((<$>))

import Control.Monad.State

type Err = Either String

type Label = Int
type Temp = Int
type Name = String

data CodeGenState = CodeGenState
  { nextTemp      :: Temp
  , nextLabel     :: Label
  , localVars     :: Map.Map Name String
  , localVarsTypes    :: Map.Map Name String
  , functionTypes :: Map.Map Name String
  , codeAcc       :: [String]
  } deriving (Show)

initialState :: CodeGenState
initialState = CodeGenState
  { nextTemp = 0
  , nextLabel = 0
  , localVars = Map.empty
  , localVarsTypes = Map.empty
  , functionTypes = Map.empty
  , codeAcc = []
  }

type CodeGen = State CodeGenState

freshTemp :: CodeGen String
freshTemp = do
  state <- get
  let temp = nextTemp state
  put state { nextTemp = temp + 1 }
  return $ "%t" ++ show temp

freshLabel :: CodeGen String
freshLabel = do
  state <- get
  let lbl = nextLabel state
  put state { nextLabel = lbl + 1 }
  return $ "L" ++ show lbl

addLocal :: Name -> String -> CodeGen ()
addLocal name addr = do
  state <- get
  let locals = localVars state
  put state { localVars = Map.insert name addr locals }

getLocal :: Name -> CodeGen (Maybe String)
getLocal name = do
  state <- get
  return $ Map.lookup name (localVars state)

addLocalVarsType :: Name -> String -> CodeGen ()
addLocalVarsType name addr = do
  state <- get
  let locals = localVarsTypes state
  put state { localVarsTypes = Map.insert name addr locals }

getLocalVarsType :: Name -> CodeGen (Maybe String)
getLocalVarsType name = do
  state <- get
  return $ Map.lookup name (localVarsTypes state)

getFunctionType :: Name -> CodeGen (Maybe String)
getFunctionType name = do
  state <- get
  return $ Map.lookup name (functionTypes state)

emit :: String -> CodeGen ()
emit instr = modify (\s -> s { codeAcc = instr : codeAcc s })

flushCode :: CodeGen [String]
flushCode = do
  state <- get
  let code = codeAcc state
  modify (\s -> s { codeAcc = [] })
  return code

runCodeGen :: CodeGenState -> CodeGen a -> (a, CodeGenState)
runCodeGen initialState codeGen = runState codeGen initialState

generatePredefinedFunctions :: [String]
generatePredefinedFunctions =
  [ "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()\n"
  ]

generateLLVM :: Program -> CodeGen String
generateLLVM program = do
  let predefinedCode = generatePredefinedFunctions
  finalCode <- generateProgramCode program
  return $ unlines (predefinedCode ++ finalCode)


generateProgramCode :: Program -> CodeGen [String]
generateProgramCode (Program _ topDefs) = do
  functionCodes <- mapM generateFunction topDefs
  return $ concat functionCodes

generateFunction :: TopDef -> CodeGen [String]
generateFunction (FnDef _ returnType ident args block) = do
  let llvmReturnType = getType returnType
      functionName = getIdentName ident

  llvmArgs <- foldM generateFunctionArg [] args

  let header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"

  generateBlockCode block
  blockCode <- flushCode
  return $ header : reverse ("}\n" : blockCode)



generateFunctionArg :: [String] -> Arg -> CodeGen [String]
generateFunctionArg argsCode (Arg _ argType ident) = do
  let llvmType = getType argType
      argName = getIdentName ident
  addLocal argName ("%" ++ argName)
  addLocalVarsType argName llvmType
  return (argsCode ++ [llvmType ++ " %" ++ argName])

getIdentName :: Ident -> String
getIdentName (Ident name) = name

getType :: Type -> String
getType (Int _) = "i32"
getType (Str _) = "i8*"
getType (Bool _) ="i1"
getType (Void _) = "void"
getType (Fun _ returnType paramTypes) = getType returnType

generateBlockCode :: Block -> CodeGen ()
generateBlockCode (Block _ stmts) = do
  mapM_ processStmt stmts


processStmt :: Stmt -> CodeGen ()
processStmt (Decl _ varType items) = do
  mapM_ (generateVarDecl varType) items
processStmt (BStmt _ (Block _ stmts)) = do
  mapM_ processStmt (reverse stmts)
processStmt (Ass _ ident expr) = do
  exprCode <- generateExprCode expr
  let variableName = "%" ++ getIdentName ident
      assignCode = "  " ++ variableName ++ " = " ++ exprCode
  emit assignCode
processStmt (Incr _ ident) = do
  let variableName = "%" ++ getIdentName ident
      incrCode = "  " ++ variableName ++ " = add i32 " ++ variableName ++ ", 1"
  emit incrCode
processStmt (Decr _ ident) = do
  let variableName = "%" ++ getIdentName ident
      decrCode = "  " ++ variableName ++ " = sub i32 " ++ variableName ++ ", 1"
  emit decrCode
processStmt (Ret _ expr) = do
  exprCode <- generateExprCode expr
  exprType <- checkExprType expr
  let retCode = "  ret " ++ exprType ++ " " ++ exprCode
  emit retCode
processStmt (VRet _) = do
  emit "  ret void"
processStmt (SExp _ expr) = do
  exprCode <- generateExprCode expr
  emit exprCode

processStmt (CondElse _ cond trueStmt falseStmt) = do
  c <- generateExprCode cond
  
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  
  let trueLabel' = trueLabel ++ "_if_true"
      falseLabel' = falseLabel ++ "_if_false"
      endLabel' = endLabel ++ "_if_end"
  
  emit $ "  br i1 " ++ c ++ ", label %" ++ trueLabel' ++ ", label %" ++ falseLabel'
  
  emit $ trueLabel' ++ ":" 
  processStmt trueStmt
  emit $ "  br label %" ++ endLabel'
  
  emit $ falseLabel' ++ ":"
  processStmt falseStmt
  emit $ "  br label %" ++ endLabel'

  emit $ endLabel' ++ ":"

processStmt (Cond _ cond stmt) = do
  c <- generateExprCode cond
  
  trueLabel <- freshLabel
  endLabel <- freshLabel
  
  let trueLabel' = trueLabel ++ "_cond_true"
      endLabel' = endLabel ++ "_cond_end"
  
  emit $ "  br i1 " ++ c ++ ", label %" ++ trueLabel' ++ ", label %" ++ endLabel'
  emit $ trueLabel' ++ ":"
  
  processStmt stmt  
  emit $ "  br label %" ++ endLabel'
  emit $ endLabel' ++ ":"

processStmt (While _ cond stmt) = do
  startLabel <- freshLabel
  bodyLabel <- freshLabel
  endLabel <- freshLabel
  
  let startLabel' = startLabel ++ "_while_cond"
      bodyLabel' = bodyLabel ++ "_while_body"
      endLabel' = endLabel ++ "_while_end"

  emit $ "  br label %" ++ startLabel'
  emit $ bodyLabel' ++ ":"
  processStmt stmt
  
  emit $ "  br label %" ++ startLabel'
  emit $ startLabel' ++ ":"
  condVal <- generateExprCode cond

  emit $ "  br i1 " ++ condVal ++ ", label %" ++ bodyLabel' ++ ", label %" ++ endLabel' 

  emit $ endLabel' ++ ":"

processStmt _ = return ()

generateVarDecl :: Type -> Item -> CodeGen ()
generateVarDecl varType (NoInit _ ident) = do
  let variableName = "%" ++ getIdentName ident
      llvmType = getType varType
      defaultValue = getDefaultForType varType
      code = "  " ++ variableName ++ " = " ++ defaultValue
  addLocal (getIdentName ident) variableName
  addLocalVarsType (getIdentName ident) llvmType
  emit code

generateVarDecl varType (Init _ ident expr) = do
  exprCode <- generateExprCode expr
  let variableName = "%" ++ getIdentName ident
      llvmType = getType varType
      code = "  " ++ variableName ++ " = " ++ exprCode
  addLocal (getIdentName ident) variableName
  addLocalVarsType (getIdentName ident) llvmType
  emit code

generateExprCode :: Expr -> CodeGen String
generateExprCode (ELitInt _ value) = return $ show value
generateExprCode (EString _ value) = return $ "c\"" ++ escapeString value ++ "\""
generateExprCode (EVar _ ident) = do
  maybeAddr <- getLocal (getIdentName ident)
  case maybeAddr of
    Just addr -> return addr
    Nothing -> error $ "Variable " ++ getIdentName ident ++ " not found"
generateExprCode (ELitTrue _) = return "1"
generateExprCode (ELitFalse _) = return "0"
generateExprCode (EApp _ ident args) = do
  argsCode <- mapM generateExprCode args
  let functionName = "@" ++ getIdentName ident
  return $ "  call " ++ functionName ++ "(" ++ intercalate ", " argsCode ++ ")"
generateExprCode (Neg _ expr) = do
  exprCode <- generateExprCode expr
  return $ "  sub i32 0, " ++ exprCode
generateExprCode (Not _ expr) = do
  exprCode <- generateExprCode expr
  return $ "  xor i1 " ++ exprCode ++ ", 1"
generateExprCode (EMul _ expr1 op expr2) = do
  let llvmOp = case op of
        Times _ -> "mul i32"
        Div _   -> "sdiv i32"
        Mod _   -> "srem i32"
  genBinOp llvmOp expr1 expr2
generateExprCode (EAdd _ expr1 op expr2) = do
  let llvmOp = case op of
        Plus _  -> "add i32"
        Minus _ -> "sub i32"
  genBinOp llvmOp expr1 expr2
generateExprCode (ERel _ expr1 op expr2) = do
  lhsType <- checkExprType expr1
  let llvmOp = case (lhsType, op) of
        ("i32", LTH _) -> "icmp slt i32"
        ("i32", LE  _) -> "icmp sle i32"
        ("i32", GTH _) -> "icmp sgt i32"
        ("i32", GE  _) -> "icmp sge i32"
        ("i32", EQU _) -> "icmp eq i32"
        ("i32", NE  _) -> "icmp ne i32"
        ("i1", EQU _)  -> "icmp eq i1"
        ("i1", NE  _)  -> "icmp ne i1"
        ("i8*", EQU _) -> "icmp eq i8*"
        ("i8*", NE  _) -> "icmp ne i8*"
        _ -> error ("Unsupported relational operation for type: " ++ lhsType)
  genBinOp llvmOp expr1 expr2
generateExprCode (EAnd _ expr1 expr2) =
  genBinOp "and i1" expr1 expr2
generateExprCode (EOr _ expr1 expr2) =
  genBinOp "or i1" expr1 expr2

genBinOp :: String -> Expr -> Expr -> CodeGen String
genBinOp llvmOp expr1 expr2 = do
  lhsCode <- generateExprCode expr1  
  rhsCode <- generateExprCode expr2 
  tempVar <- freshTemp 
  let instr = "  " ++ tempVar ++ " = " ++ llvmOp ++ " " ++ lhsCode ++ ", " ++ rhsCode
  emit instr
  return tempVar

checkExprType :: Expr -> CodeGen String
checkExprType (ELitInt {}) = return "i32"
checkExprType (EString {}) = return "i8*"
checkExprType (EVar _ ident) = do
  maybeType <- getLocalVarsType (getIdentName ident)
  case maybeType of
    Just varType -> return varType
    Nothing -> error $ "Variable " ++ getIdentName ident ++ " not found"
checkExprType (EApp _ ident _) = do
  maybeType <- getFunctionType (getIdentName ident)
  case maybeType of
    Just returnType -> return returnType
    Nothing -> error $ "Function " ++ getIdentName ident ++ " not found"
checkExprType (ELitTrue _) = return "i1"
checkExprType (ELitFalse _) = return "i1"
checkExprType (Neg {}) = return "i32"
checkExprType (Not {}) = return "i1"
checkExprType (EMul {}) = return "i32"
checkExprType (EAdd _ arg1 _ _) = checkExprType arg1
checkExprType (ERel _ arg1 _ _) = return "i1"
checkExprType (EAnd {}) = return "i1"
checkExprType (EOr {}) = return "i1"

getDefaultForType :: Type -> String
getDefaultForType (Int _) = "0"
getDefaultForType (Str _) = "\"\""
getDefaultForType (Bool _) = "false"
getDefaultForType _ = error "Unsupported type for default value"

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]
