{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Frontend where

import AbsLatte
import qualified AbsLatte
import qualified AbsLatte as Map
import Control.Monad (foldM, mapM)
import qualified Data.Map as Map
import Prelude (Bool (..), Either (..), Eq, Int, Integer, Maybe (..), Show, String, any, div, error, foldl, head, length, map, mod, not, null, otherwise, return, show, unwords, ($), (&&), (*), (+), (++), (-), (/=), (<), (<=), (==), (>), (>=), (^), (||))

type Err = Either String

type Result = Err String

type VarEnvResult = Err VarEnv

data FuncType = FuncType String [String] deriving (Show)

type FunEnv = Map.Map String FuncType

type VarEnv = Map.Map String String

intMinValue :: Integer
intMinValue = -(2 ^ 31)

intMaxValue :: Integer
intMaxValue = 2 ^ 31 - 1

builtinFunctions :: FunEnv
builtinFunctions =
  Map.fromList
    [ ("printInt", FuncType "void" ["int"]),
      ("printString", FuncType "void" ["string"]),
      ("error", FuncType "void" []),
      ("readInt", FuncType "int" []),
      ("readString", FuncType "string" [])
    ]

checkSemantics :: Program -> Result
checkSemantics program = do
  let collectResult = collectFunctionTypes program
  case collectResult of
    Left err -> Left err
    Right env -> do
      transProgramResult <- transProgram program env
      case Map.lookup "main" env of
        Nothing -> Left "Function 'main' is not defined."
        Just _ -> Right transProgramResult

collectFunctionTypes :: Program -> Err FunEnv
collectFunctionTypes (Program _ topdefs) = foldM addFunctionToEnv builtinFunctions topdefs

addFunctionToEnv :: FunEnv -> TopDef -> Err FunEnv
addFunctionToEnv env (FnDef pos returnType ident args _) = do
  let funcName = getIdentName ident
      argTypes = map getArgType args
  if Map.member funcName env
    then
      let message = "Function '" ++ funcName ++ "' is already declared."
       in Left (positionErrorDirectPos message pos)
    else do
      if funcName == "main"
        then do
          if isValidMainReturn returnType
            then
              if not (isValidMainArgs argTypes)
                then Left (positionError "Function 'main' must take no arguments." (head argTypes))
                else return ()
            else Left (positionError "Function 'main' must return 'int'." returnType)
        else return ()

      let formattedArgTypes = map getType argTypes
          formattedReturnType = getType returnType
          funcType = FuncType formattedReturnType formattedArgTypes
          updatedEnv = Map.insert funcName funcType env
      Right updatedEnv

getIdentName :: Ident -> String
getIdentName (Ident name) = name

getArgType :: Arg -> Type
getArgType (Arg _ argType _) = argType

isValidMainReturn :: Type -> Bool
isValidMainReturn (Int _) = True
isValidMainReturn _ = False

isValidMainArgs :: [Type] -> Bool
isValidMainArgs argTypes = null argTypes

positionError :: String -> (HasPosition a) => a -> String
positionError message entity = do
  case hasPosition entity of
    Just pos -> "Error at position " ++ show pos ++ ": " ++ message
    Nothing -> message

positionErrorDirectPos :: String -> BNFC'Position -> String
positionErrorDirectPos message pos =
  case getPositionPair pos of
    Just (x, y) -> "Error at position (" ++ show x ++ ", " ++ show y ++ "): " ++ message
    Nothing -> message

getPositionPair :: BNFC'Position -> Maybe (Int, Int)
getPositionPair (Just (x, y)) = Just (x, y)
getPositionPair Nothing = Nothing

transProgram :: Program -> FunEnv -> Result
transProgram (Program _ topdefs) env = checkTopDefs topdefs env

checkTopDefs :: [TopDef] -> FunEnv -> Result
checkTopDefs [] env = Right "OK"
checkTopDefs (topdef : rest) env = do
  case transTopDef topdef env of
    Left err -> Left err
    Right _ -> checkTopDefs rest env

transTopDef :: TopDef -> FunEnv -> Result
transTopDef (FnDef pos _ ident args block) funEnv = do
  let varEnv = Map.empty
  case getReturnType (getIdentName ident) funEnv of
    Nothing -> Left ("Function '" ++ getIdentName ident ++ "' not found in environment.")
    Just returnType ->
      case checkArgs args varEnv of
        Left err -> Left err
        Right updatedEnv -> do
          checkBlockResult <- checkBlock block updatedEnv Map.empty funEnv returnType
          if containsGuaranteedReturnBlock block
            then Right "OK"
            else
              if returnType /= "void"
                then Left (positionErrorDirectPos ("Missing guaranteed return statement in function " ++ getIdentName ident) pos)
                else Right "OK"

getReturnType :: String -> FunEnv -> Maybe String
getReturnType funcName funcEnv = case Map.lookup funcName funcEnv of
  Just (FuncType returnType _) -> Just returnType
  Nothing -> Nothing

checkArgs :: [Arg] -> VarEnv -> VarEnvResult
checkArgs [] varEnv = Right varEnv
checkArgs (Arg pos argType ident : rest) varEnv = do
  let varName = getIdentName ident
  if Map.member varName varEnv
    then
      let message = "Argument '" ++ varName ++ "' is already declared."
       in Left (positionErrorDirectPos message pos)
    else case argType of
      Void _ -> Left (positionErrorDirectPos "Function arguments cannot have 'void' type." pos)
      _ -> do
        let updatedEnv = Map.insert varName (getType argType) varEnv
        checkArgs rest updatedEnv

checkBlock :: Block -> VarEnv -> VarEnv -> FunEnv -> String -> Result
checkBlock (Block _ stmts) localVarEnv globalVarEnv funEnv returnType = do
  case traverseStmts stmts localVarEnv globalVarEnv funEnv returnType of
    Left err -> Left err
    Right _ -> Right "OK"

containsGuaranteedReturn :: Stmt -> Bool
containsGuaranteedReturn (Ret _ _) = True
containsGuaranteedReturn (VRet _) = True
containsGuaranteedReturn (BStmt _ block) = containsGuaranteedReturnBlock block
containsGuaranteedReturn (CondElse _ expr stmt1 stmt2) =
  if isStaticExpr expr
    then
      if evalStaticExpr expr stmt1
        then
          containsGuaranteedReturn stmt1
        else
          containsGuaranteedReturn stmt2
    else
      containsGuaranteedReturn stmt1 && containsGuaranteedReturn stmt2
containsGuaranteedReturn (Cond _ expr stmt) =
  (isStaticExpr expr && evalStaticExpr expr stmt)
    && containsGuaranteedReturn stmt
containsGuaranteedReturn (While _ expr stmt) =
  (isStaticExpr expr && evalStaticExpr expr stmt)
    && containsGuaranteedReturn stmt
-- error() nadpisuje return
containsGuaranteedReturn (SExp _ (EApp _ (Ident "error") _)) = True
containsGuaranteedReturn _ = False

containsGuaranteedReturnBlock :: Block -> Bool
containsGuaranteedReturnBlock (Block _ stmts) = any containsGuaranteedReturn stmts

isStaticExpr :: Expr -> Bool
isStaticExpr (ELitTrue _) = True
isStaticExpr (ELitFalse _) = True
isStaticExpr (ELitInt _ _) = True
isStaticExpr (EString _ _) = True
isStaticExpr (Not _ inner) = isStaticExpr inner
isStaticExpr (Neg _ inner) = isStaticExpr inner
isStaticExpr (EMul _ left _ right) = isStaticExpr left && isStaticExpr right
isStaticExpr (EAdd _ left _ right) = isStaticExpr left && isStaticExpr right
isStaticExpr (ERel _ left _ right) = isStaticExpr left && isStaticExpr right
isStaticExpr (EAnd _ expr1 expr2) = isStaticExpr expr1 && isStaticExpr expr2
isStaticExpr (EOr _ expr1 expr2) = isStaticExpr expr1 || isStaticExpr expr2
isStaticExpr _ = False

evalStaticExpr :: Expr -> Stmt -> Bool
evalStaticExpr expr stmt =
  case expr of
    ELitTrue _ -> True
    ELitFalse _ -> False
    Not _ innerExpr ->
      case evalStaticExprBool innerExpr of
        Just False -> containsGuaranteedReturn stmt -- `!false`
        _ -> False
    ERel _ left relOp right ->
      case (evalStaticExprBool left, evalStaticExprBool right) of
        (Just val1, Just val2) ->
          evalRelExprBool relOp val1 val2 && containsGuaranteedReturn stmt
        _ -> case (evalStaticExprInt left, evalStaticExprInt right) of
          (Just val1, Just val2) ->
            evalRelExprInt relOp val1 val2 && containsGuaranteedReturn stmt
          _ -> case (evalStaticExprString left, evalStaticExprString right) of
            (Just val1, Just val2) ->
              evalRelExprString relOp val1 val2 && containsGuaranteedReturn stmt
            _ -> False
    EAnd _ expr1 expr2 ->
      case (evalStaticExprBool expr1, evalStaticExprBool expr2) of
        (Just val1, Just val2) ->
          case (val1, val2) of
            (True, True) -> containsGuaranteedReturn stmt
            _ -> False
        _ -> False
    EOr _ expr1 expr2 ->
      case (evalStaticExprBool expr1, evalStaticExprBool expr2) of
        (Just val1, Just val2) ->
          case (val1, val2) of
            (True, _) -> containsGuaranteedReturn stmt
            (_, True) -> containsGuaranteedReturn stmt
            _ -> False
        _ -> False
    _ -> False

evalStaticExprBool :: Expr -> Maybe Bool
evalStaticExprBool (ELitTrue _) = Just True
evalStaticExprBool (ELitFalse _) = Just False
evalStaticExprBool (Not _ inner) =
  case evalStaticExprBool inner of
    Just True -> Just False
    Just False -> Just True
    _ -> Nothing
evalStaticExprBool (ERel _ left relop right) =
  case (evalStaticExprBool left, evalStaticExprBool right) of
    (Just val1, Just val2) ->
      Just (evalRelExprBool relop val1 val2)
    _ -> case (evalStaticExprInt left, evalStaticExprInt right) of
      (Just val1, Just val2) ->
        Just (evalRelExprInt relop val1 val2)
      _ -> case (evalStaticExprString left, evalStaticExprString right) of
        (Just val1, Just val2) ->
          Just (evalRelExprString relop val1 val2)
        _ -> Nothing
evalStaticExprBool (EAnd _ left right) =
  case (evalStaticExprBool left, evalStaticExprBool right) of
    (Just True, Just True) -> Just True
    _ -> Just False
evalStaticExprBool (EOr _ left right) =
  case (evalStaticExprBool left, evalStaticExprBool right) of
    (Just False, Just False) -> Just False
    _ -> Just True
evalStaticExprBool _ = Nothing

evalStaticExprInt :: Expr -> Maybe Integer
evalStaticExprInt (ELitInt _ n) = Just n
evalStaticExprInt (Neg _ inner) =
  case evalStaticExprInt inner of
    Just n -> Just (-n)
    _ -> Nothing
evalStaticExprInt (EMul _ left mulop right) =
  case (evalStaticExprInt left, evalStaticExprInt right) of
    (Just val1, Just val2) -> Just (evalMulOp mulop val1 val2)
    _ -> Nothing
evalStaticExprInt (EAdd _ left addop right) =
  case (evalStaticExprInt left, evalStaticExprInt right) of
    (Just val1, Just val2) -> Just (evalAddOp addop val1 val2)
    _ -> Nothing
evalStaticExprInt _ = Nothing

evalStaticExprString :: Expr -> Maybe String
evalStaticExprString (EString _ n) = Just n
evalStaticExprString (EAdd _ left addop right) =
  case (evalStaticExprString left, evalStaticExprString right) of
    (Just val1, Just val2) -> Just (evalAddOpString addop val1 val2)
    _ -> Nothing
evalStaticExprString _ = Nothing

evalRelExprBool :: RelOp -> Bool -> Bool -> Bool
evalRelExprBool (EQU _) val1 val2 = val1 == val2
evalRelExprBool (NE _) val1 val2 = val1 /= val2
evalRelExprBool _ _ _ = False

evalRelExprString :: RelOp -> String -> String -> Bool
evalRelExprString (EQU _) val1 val2 = val1 == val2
evalRelExprString (NE _) val1 val2 = val1 /= val2
evalRelExprString _ _ _ = False

evalRelExprInt :: RelOp -> Integer -> Integer -> Bool
evalRelExprInt relOp val1 val2 = case relOp of
  EQU _ -> val1 == val2
  LTH _ -> val1 < val2
  LE _ -> val1 <= val2
  GTH _ -> val1 > val2
  GE _ -> val1 >= val2
  NE _ -> val1 /= val2

evalMulOp :: MulOp -> Integer -> Integer -> Integer
evalMulOp (Times _) val1 val2 = val1 * val2
evalMulOp (Div pos) val1 val2 =
  if val2 == 0
    then error (positionErrorDirectPos "Division by zero in static expression." pos)
    else val1 `div` val2
evalMulOp (Mod pos) val1 val2 =
  if val2 == 0
    then error (positionErrorDirectPos "Modulo by zero in static expression." pos)
    else val1 `mod` val2

evalAddOp :: AddOp -> Integer -> Integer -> Integer
evalAddOp (Plus _) val1 val2 = val1 + val2
evalAddOp (Minus _) val1 val2 = val1 - val2

evalAddOpString :: AddOp -> String -> String -> String
evalAddOpString (Plus _) val1 val2 = val1 ++ val2

traverseStmts :: [Stmt] -> VarEnv -> VarEnv -> FunEnv -> String -> VarEnvResult
traverseStmts [] localVarEnv globalVarEnv _ _ = Right localVarEnv
traverseStmts (stmt : rest) localVarEnv globalVarEnv funEnv returnType = do
  let stmtResponse = checkStmt stmt localVarEnv globalVarEnv funEnv returnType
  case stmtResponse of
    Left err -> Left err
    Right newVarEnv -> traverseStmts rest newVarEnv globalVarEnv funEnv returnType

getType :: Type -> String
getType (Int _) = "int"
getType (Str _) = "string"
getType (Bool _) = "bool"
getType (Void _) = "void"
getType (Fun _ returnType paramTypes) = getType returnType

lookupVar :: String -> VarEnv -> VarEnv -> Maybe String
lookupVar varName localVarEnv globalVarEnv =
  case Map.lookup varName localVarEnv of
    Just varType -> Just varType
    Nothing ->
      Map.lookup varName globalVarEnv

checkStmt :: Stmt -> VarEnv -> VarEnv -> FunEnv -> String -> VarEnvResult
checkStmt (Empty _) localVarEnv globalVarEnv _ _ = Right localVarEnv
checkStmt (Decl pos varType items) localVarEnv globalVarEnv funEnv _ = do
  if getType varType == "void"
    then Left (positionErrorDirectPos "Cannot declare a variable with type 'void'." pos)
    else processItems pos (getType varType) items localVarEnv globalVarEnv funEnv
checkStmt (Ass pos ident expr) localVarEnv globalVarEnv funEnv returnType = do
  let varName = getIdentName ident
  case lookupVar varName localVarEnv globalVarEnv of
    Nothing -> Left (positionErrorDirectPos ("Variable '" ++ varName ++ "' is not declared.") pos)
    Just varType -> do
      exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
      if exprType == varType
        then Right localVarEnv
        else
          Left
            ( positionErrorDirectPos
                ( "Type mismatch in assignment to variable '"
                    ++ varName
                    ++ "'.\nExpected: "
                    ++ varType
                    ++ "\nGot: "
                    ++ exprType
                )
                pos
            )
checkStmt (BStmt _ block) localVarEnv globalVarEnv funEnv returnType = do
  let combinedEnv = Map.union localVarEnv globalVarEnv
  checkBlock block Map.empty combinedEnv funEnv returnType
  Right localVarEnv
checkStmt (Incr pos ident) localVarEnv globalVarEnv _ _ = do
  let varName = getIdentName ident
  case lookupVar varName localVarEnv globalVarEnv of
    Just varType ->
      if varType == "int"
        then Right localVarEnv
        else Left (positionErrorDirectPos ("Increment requires an integer, but '" ++ varName ++ "' is of type " ++ varType ++ ".") pos)
    Nothing -> Left (positionErrorDirectPos ("Variable '" ++ varName ++ "' is not declared.") pos)
checkStmt (Decr pos ident) localVarEnv globalVarEnv _ _ = do
  let varName = getIdentName ident
  case lookupVar varName localVarEnv globalVarEnv of
    Just varType ->
      if varType == "int"
        then Right localVarEnv
        else Left (positionErrorDirectPos ("Decrement requires an integer, but '" ++ varName ++ "' is of type " ++ varType ++ ".") pos)
    Nothing -> Left (positionErrorDirectPos ("Variable '" ++ varName ++ "' is not declared.") pos)
checkStmt (Cond pos expr stmt) localVarEnv globalVarEnv funEnv returnType = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  case exprType of
    "bool" -> do
      let combinedEnv = Map.union localVarEnv globalVarEnv
      checkStmt stmt Map.empty combinedEnv funEnv returnType
      return localVarEnv
    _ ->
      Left (positionError "Condition must be of type 'Bool'." expr)
checkStmt (CondElse pos expr stmt1 stmt2) localVarEnv globalVarEnv funEnv returnType = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  if exprType /= "bool"
    then Left (positionErrorDirectPos "Condition expression must be of type Bool." pos)
    else do
      let combinedEnv = Map.union localVarEnv globalVarEnv
      checkStmt stmt1 Map.empty combinedEnv funEnv returnType
      checkStmt stmt2 Map.empty combinedEnv funEnv returnType
      Right localVarEnv
checkStmt (While pos expr stmt) localVarEnv globalVarEnv funEnv returnType = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  if exprType /= "bool"
    then Left (positionErrorDirectPos "While loop condition must be of type Bool." pos)
    else do
      let combinedEnv = Map.union localVarEnv globalVarEnv
      checkStmt stmt Map.empty combinedEnv funEnv returnType
      Right localVarEnv
checkStmt (SExp pos expr) localVarEnv globalVarEnv funEnv returnType = do
  _ <- checkExprType expr localVarEnv globalVarEnv funEnv
  Right localVarEnv
checkStmt (Ret pos expr) localVarEnv globalVarEnv funEnv returnType = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  if exprType == returnType
    then Right localVarEnv
    else
      Left
        ( positionErrorDirectPos
            ( "Return type mismatch. Expected: "
                ++ returnType
                ++ ", Got: "
                ++ exprType
            )
            pos
        )
checkStmt (VRet pos) localVarEnv globalVarEnv funEnv returnType =
  if returnType == "void"
    then Right localVarEnv
    else Left (positionErrorDirectPos "Return without value in a non-void function." pos)

processItems :: BNFC'Position -> String -> [Item] -> VarEnv -> VarEnv -> FunEnv -> VarEnvResult
processItems _ _ [] localVarEnv _ _ = Right localVarEnv
processItems pos varType (item : rest) localVarEnv globalVarEnv funEnv = do
  let processItemResponse = processItem pos varType item localVarEnv globalVarEnv funEnv
  case processItemResponse of
    Left err -> Left err
    Right newVarEnv -> processItems pos varType rest newVarEnv globalVarEnv funEnv

processItem :: BNFC'Position -> String -> Item -> VarEnv -> VarEnv -> FunEnv -> VarEnvResult
processItem pos varType item localVarEnv globalVarEnv funEnv = do
  let varName = case item of
        NoInit _ ident -> getIdentName ident
        Init _ ident _ -> getIdentName ident
  if Map.member varName localVarEnv
    then Left (positionErrorDirectPos ("Variable '" ++ varName ++ "' already declared.") pos)
    else case item of
      NoInit _ _ -> Right (Map.insert varName varType localVarEnv)
      Init initPos _ expr -> do
        exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
        if exprType == varType
          then Right (Map.insert varName varType localVarEnv)
          else Left (positionError ("Type mismatch in initialization of variable '" ++ varName ++ "'. \nExpected: " ++ show varType ++ "\nGot: " ++ show exprType) expr)

getVariableType :: Ident -> VarEnv -> VarEnv -> Err String
getVariableType ident localVarEnv globalVarEnv = do
  case lookupVar (getIdentName ident) localVarEnv globalVarEnv of
    Just varType -> Right varType
    Nothing -> Left ("Variable '" ++ getIdentName ident ++ "' not declared.")

checkExprType :: Expr -> VarEnv -> VarEnv -> FunEnv -> Err String
checkExprType (EVar pos ident) localVarEnv globalVarEnv _ = do
  case getVariableType ident localVarEnv globalVarEnv of
    Left err -> Left (positionErrorDirectPos err pos)
    Right varType -> Right varType
checkExprType (ELitInt pos value) _ _ _
  | value < intMinValue || value > intMaxValue =
      Left (positionErrorDirectPos "Integer literal out of range for type 'int'." pos)
  | otherwise =
      Right "int"
checkExprType (ELitTrue _) _ _ _ = Right "bool"
checkExprType (ELitFalse _) _ _ _ = Right "bool"
checkExprType (EString _ _) _ _ _ = Right "string"
checkExprType (Neg pos expr) localVarEnv globalVarEnv funEnv = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  case exprType of
    "int" -> Right "int"
    _ -> Left (positionErrorDirectPos "Negation requires an integer." pos)
checkExprType (Not pos expr) localVarEnv globalVarEnv funEnv = do
  exprType <- checkExprType expr localVarEnv globalVarEnv funEnv
  case exprType of
    "bool" -> Right "bool"
    _ -> Left (positionErrorDirectPos "'Not' operator requires a boolean." pos)
checkExprType (EApp pos ident args) localVarEnv globalVarEnv funEnv = do
  let funcName = getIdentName ident
  case Map.lookup funcName funEnv of
    Nothing -> Left (positionErrorDirectPos ("Function '" ++ funcName ++ "' not declared.") pos)
    Just (FuncType returnType paramTypes) -> do
      validateFunctionArgs args paramTypes localVarEnv globalVarEnv funEnv pos
      Right returnType
checkExprType (EMul pos expr1 mulop expr2) localVarEnv globalVarEnv funEnv = do
  expr1Type <- checkExprType expr1 localVarEnv globalVarEnv funEnv
  expr2Type <- checkExprType expr2 localVarEnv globalVarEnv funEnv
  case (expr1Type, expr2Type) of
    ("int", "int") -> case (evalStaticExprInt expr1, evalStaticExprInt expr2) of
      (Just value1, Just value2) ->
        case mulop of
          Times _ ->
            if value1 * value2 > intMaxValue || value1 * value2 < intMinValue
              then Left (positionErrorDirectPos "Integer multiplication out of range for type 'int'." pos)
              else Right "int"
          Div _ ->
            if value2 == 0
              then Left (positionErrorDirectPos "Division by zero." pos)
              else Right "int"
          Mod _ ->
            if value2 == 0
              then Left (positionErrorDirectPos "Modulo by zero." pos)
              else Right "int"
      _ -> Right "int"
    _ -> Left (positionErrorDirectPos "Multiplication requires two integers." pos)
checkExprType (EAdd pos expr1 addop expr2) localVarEnv globalVarEnv funEnv = do
  expr1Type <- checkExprType expr1 localVarEnv globalVarEnv funEnv
  expr2Type <- checkExprType expr2 localVarEnv globalVarEnv funEnv
  case (expr1Type, expr2Type) of
    ("int", "int") ->
      case (evalStaticExprInt expr1, evalStaticExprInt expr2) of
        (Just value1, Just value2) ->
          case addop of
            Plus _ ->
              if value1 + value2 > intMaxValue || value1 + value2 < intMinValue
                then Left (positionErrorDirectPos "Integer addition out of range for type 'int'." pos)
                else Right "int"
            Minus _ ->
              if value1 - value2 > intMaxValue || value1 - value2 < intMinValue
                then Left (positionErrorDirectPos "Integer subtraction out of range for type 'int'." pos)
                else Right "int"
        _ -> Right "int"
    ("string", "string") ->
      case addop of
        Plus _ -> Right "string"
        Minus _ -> Left (positionErrorDirectPos "Subtraction is not allowed for strings." pos)
    _ -> Left (positionErrorDirectPos "Addition or subtraction requires matching types" pos)
checkExprType (ERel pos expr1 relop expr2) localVarEnv globalVarEnv funEnv = do
  expr1Type <- checkExprType expr1 localVarEnv globalVarEnv funEnv
  expr2Type <- checkExprType expr2 localVarEnv globalVarEnv funEnv
  case (expr1Type, expr2Type) of
    ("int", "int") -> Right "bool"
    ("bool", "bool") ->
      case relop of
        EQU _ -> Right "bool"
        NE _ -> Right "bool"
        _ -> Left (positionErrorDirectPos "Invalid relational operator for bools (only == or != are allowed)." pos)
    ("string", "string") ->
      case relop of
        EQU _ -> Right "bool"
        NE _ -> Right "bool"
        _ -> Left (positionErrorDirectPos "Invalid relational operator for strings (only == or != are allowed)." pos)
    _ -> Left (positionErrorDirectPos "Relational operators require both operands to be of the same type (Int, Bool, or String)." pos)
checkExprType (EAnd pos expr1 expr2) localVarEnv globalVarEnv funEnv = do
  expr1Type <- checkExprType expr1 localVarEnv globalVarEnv funEnv
  expr2Type <- checkExprType expr2 localVarEnv globalVarEnv funEnv
  case (expr1Type, expr2Type) of
    ("bool", "bool") -> Right "bool"
    _ -> Left (positionErrorDirectPos "'AND' operator requires both operands to be of type 'Bool'." pos)
checkExprType (EOr pos expr1 expr2) localVarEnv globalVarEnv funEnv = do
  expr1Type <- checkExprType expr1 localVarEnv globalVarEnv funEnv
  expr2Type <- checkExprType expr2 localVarEnv globalVarEnv funEnv
  case (expr1Type, expr2Type) of
    ("bool", "bool") -> Right "bool"
    _ -> Left (positionErrorDirectPos "'OR' operator requires both operands to be of type 'Bool'." pos)

validateFunctionArgs :: [Expr] -> [String] -> VarEnv -> VarEnv -> FunEnv -> BNFC'Position -> Err ()
validateFunctionArgs args paramTypes localVarEnv globalVarEnv funEnv pos = do
  if length args /= length paramTypes
    then
      Left
        ( positionErrorDirectPos
            ( "Incorrect number of arguments in function call. "
                ++ "Expected "
                ++ show (length paramTypes)
                ++ ", got "
                ++ show (length args)
                ++ "."
            )
            pos
        )
    else do
      argTypes <- mapM (\arg -> checkExprType arg localVarEnv globalVarEnv funEnv) args
      if argTypes == paramTypes
        then Right ()
        else
          Left
            ( positionErrorDirectPos
                ( "Argument type mismatch in function call.\n"
                    ++ "Expected types: "
                    ++ show paramTypes
                    ++ "\n"
                    ++ "Got types: "
                    ++ show argTypes
                )
                pos
            )
