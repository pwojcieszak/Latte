{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf)
import Data.Functor ((<$>))
import Data.Char (isSpace, isAlphaNum)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, noneOf, spaces, char, string, between, sepBy, space, many, anyToken, skipMany, alphaNum, (<|>))
import Data.Maybe (fromMaybe)


import Control.Monad.State

type Err = Either String

type Label = Int
type Temp = Int
type Name = String
type FlowGraph = Map.Map String [String]


data CodeGenState = CodeGenState
  { nextTemp      :: Temp
  , nextLabel     :: Label
  , localVars     :: Map.Map Name String
  , localVarsTypes    :: Map.Map Name String
  , functionTypes :: Map.Map Name String
  , codeAcc       :: [String]
  , flowGraph :: FlowGraph
  , currentLabel  :: String
  , variableVersions :: Map.Map String (Map.Map String [String])
  } deriving (Show)

initialState :: CodeGenState
initialState = CodeGenState
  { nextTemp = 0
  , nextLabel = 0
  , localVars = Map.empty
  , localVarsTypes = Map.empty
  , functionTypes = Map.fromList [("printInt", "void"), ("printString", "void"), ("error", "void"), ("readInt", "i32"), ("readString", "i8*")]
  , codeAcc = []
  , flowGraph = Map.empty
  , currentLabel  = ""
  , variableVersions = Map.empty
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

addFunctionType :: Name -> String -> CodeGen ()
addFunctionType name ftype = do
  state <- get
  let functions = functionTypes state
  put state { functionTypes = Map.insert name ftype functions }

getLocalVarsType :: Name -> CodeGen String
getLocalVarsType name = do
  state <- get
  case Map.lookup name (localVarsTypes state) of
    Just localVarType -> return localVarType
    Nothing -> error $ "Local var type not found for: " ++ name

getFunctionType :: Name -> CodeGen String
getFunctionType name = do
  state <- get
  case Map.lookup name (functionTypes state) of
    Just functionType -> return functionType
    Nothing -> error $ "Function type not found for: " ++ name

clearStateExceptFunctionTypes :: CodeGen ()
clearStateExceptFunctionTypes = do 
  funTypes <- gets functionTypes
  put initialState {functionTypes = funTypes}

emit :: String -> CodeGen ()
emit instr = modify (\s -> s { codeAcc = instr : codeAcc s })

getAccCode :: CodeGen [String]
getAccCode = do
  state <- get
  let code = codeAcc state
  return code

setAccCode :: [String] -> CodeGen ()
setAccCode code = do
  state <- get
  put state { codeAcc = code }

addEdge :: String -> String -> CodeGen ()
addEdge from to = do
  state <- get
  let graph = flowGraph state
  let updatedGraph = Map.insertWith (++) from [to] graph
  put state { flowGraph = updatedGraph }

updateCurrentLabel :: String -> CodeGen ()
updateCurrentLabel newLabel = do
  state <- get
  put state { currentLabel = newLabel }

getCurrentLabel :: CodeGen String
getCurrentLabel = do
  state <- get
  return (currentLabel state)

getVariableVersion :: String -> String -> CodeGen (Maybe String)
getVariableVersion label var = do
  state <- get
  let versions = variableVersions state
  return $ do
    varMap <- Map.lookup label versions
    versionList <- Map.lookup var varMap
    return $ head versionList

collectVariableVersions :: String -> String -> CodeGen (Map.Map String [String])
collectVariableVersions label ogLabel = do
  state <- get
  let versions = variableVersions state
  let currentVars = Map.findWithDefault Map.empty label versions

  preds <- getPredecessors label
  
  if null preds || label == ogLabel then
    return currentVars
  else do
    predVars <- forM preds $ \predLabel -> do
      if null label then 
        collectVariableVersions predLabel label
      else
        collectVariableVersions predLabel ogLabel

    let predVarsMerged = Map.unions predVars
    return $ Map.union currentVars predVarsMerged

updateVariableVersion :: String -> String -> String -> CodeGen ()
updateVariableVersion label var version = do
  state <- get
  let versions = variableVersions state
      updatedVersions = Map.alter (Just . updateVarMap var version) label versions
  put state { variableVersions = updatedVersions }
  where
    updateVarMap :: String -> String -> Maybe (Map.Map String [String]) -> Map.Map String [String]
    updateVarMap var version Nothing = Map.singleton var [version]
    updateVarMap var version (Just varMap) = 
      Map.insert var (version : (Map.findWithDefault [] var varMap)) varMap

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
  mapM_ collectFunctionTypes topDefs
  functionCodes <- mapM generateFunction topDefs
  return $ concat functionCodes

collectFunctionTypes :: TopDef -> CodeGen ()
collectFunctionTypes (FnDef _ returnType ident args block) = do
  let llvmReturnType = getType returnType
      functionName = getIdentName ident
  addFunctionType functionName llvmReturnType
    
generateFunction :: TopDef -> CodeGen [String]
generateFunction (FnDef _ returnType ident args block) = do
  let llvmReturnType = getType returnType
      functionName = getIdentName ident
  clearStateExceptFunctionTypes
  addFunctionType functionName llvmReturnType

  llvmArgs <- foldM generateFunctionArg [] args

  let header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"

  entryLabel <- freshLabel
  emit $ entryLabel ++ ":"
  updateCurrentLabel entryLabel

  addArgsToLabelVarMap entryLabel args

  startLabel <- freshLabel
  emit $ "  br label %" ++ startLabel
  currentLabel <- getCurrentLabel
  addEdge currentLabel startLabel

  emit $ startLabel ++ ":"
  updateCurrentLabel startLabel

  generateBlockCode block
  blockCode <- getAccCode
  blockCodeWithSimplePhi <- processLabelsForPhi ("}\n" : blockCode)
  blockWithSimplePhiAndSSA <- renameToSSA blockCodeWithSimplePhi
  finalCode <- updatePhi blockWithSimplePhiAndSSA
  return $ header : finalCode

addArgsToLabelVarMap :: String -> [Arg] -> CodeGen ()
addArgsToLabelVarMap label args = do
  forM_ args $ \(Arg _ _ ident) -> do
    let argName = getIdentName ident
    updateVariableVersion label argName argName

processLabelsForPhi :: [String] -> CodeGen [String]
processLabelsForPhi codeLines = process codeLines []
  where
    process [] acc = return acc
    process (line:rest) acc
      | ':' `elem` line && not (null line) && last line == ':' = do
          updateCurrentLabel (init line)
          phiInstrs <- generatePhiForLabel
          process rest (line : phiInstrs ++ acc)
      | otherwise = process rest (line : acc)

generatePhiForLabel :: CodeGen [String]
generatePhiForLabel = do
    label <- getCurrentLabel
    predecessors <- getPredecessors label
    case predecessors of
      []     -> return []
      [_]    -> return []
      preds  -> do
        localVars <- gets localVarsTypes
        let phiInstrs = map (generatePhiInstr preds) (Map.toList localVars)
        return phiInstrs

generatePhiInstr :: [String] -> (Name, String) -> String
generatePhiInstr predecessors (varName, varType) =
    "  %" ++ varName ++ " = phi " ++ varType ++ " " ++ generatePhiSources predecessors varName

generatePhiSources :: [String] -> String -> String
generatePhiSources preds varName =
    intercalate ", " [ "[%" ++ varName ++ ", %" ++ pred ++ "]" | pred <- preds ]

getPredecessors :: String -> CodeGen [String]
getPredecessors label = do
  flowGraph <- gets flowGraph
  let predecessors = [predLabel | (predLabel, successors) <- Map.toList flowGraph, label `elem` successors]
  return predecessors

renameToSSA :: [String] -> CodeGen [String]
renameToSSA codeLines = process codeLines Map.empty []
  where
    process [] _ acc = return (reverse acc)
    process (line:rest) varVersions acc
      | Just (var, expr) <- parseAssignment line = do
          let newVersion = Map.findWithDefault (-1) var varVersions + 1
              newVarName = var ++ "." ++ show newVersion
              newLine = "  %" ++ newVarName ++ " = " ++ renameVariables expr varVersions
              updatedVarVersions = Map.insert var newVersion varVersions
          currentLabel <- getCurrentLabel
          updateVariableVersion currentLabel var newVarName
          process rest updatedVarVersions (newLine : acc)

      | otherwise = do
          let newLine = renameVariables line varVersions
          if not (null newLine) && last newLine == ':'
          then do
              let labelName = init newLine
              updateCurrentLabel labelName
              process rest varVersions (newLine : acc)
          else
              process rest varVersions (newLine : acc)

parseAssignment :: String -> Maybe (String, String)
parseAssignment line =
  let trimmedLine = dropWhile isSpace line
  in case span (/= '=') trimmedLine of
       ('%':varExpr, '=':rest) ->
         let var = takeWhile isValidChar varExpr
             expr = dropWhile isSpace rest
         in if not (null var) then Just (var, expr) else Nothing
       _ -> Nothing
  where
    isValidChar c = isAlphaNum c || c == '.'


renameVariables :: String -> Map.Map String Int -> String
renameVariables line varVersions =
    case parse (lineParserSSA varVersions) "" line of
        Left err  -> error $ "Parse error: " ++ show err
        Right res -> res

lineParserSSA :: Map.Map String Int -> Parser String
lineParserSSA varVersions = do
    tokens <- many (variableParserSSA varVersions <|> nonVariable)
    return $ concat tokens

variableParserSSA :: Map.Map String Int -> Parser String
variableParserSSA varVersions = do
    char '%'
    name <- many1 (alphaNum <|> char '_')
    let updated = case Map.lookup name varVersions of
                    Just version -> "%" ++ name ++ "." ++ show version
                    Nothing      -> "%" ++ name
    return updated

nonVariable :: Parser String
nonVariable = many1 (noneOf "%")

updatePhi :: [String] -> CodeGen [String]
updatePhi code = do
  forM code $ \line -> do
    if not (null line) && last line == ':' then
      let newLabel = init line in do
        updateCurrentLabel newLabel
        return line
    else if "br" `elem` words line then 
      return line
    else
      case parse phiParser "" line of
        Right (var, typ, args) -> do
          updatedArgs <- mapM (resolvePhiArg var) args
          let updatedPhi = "  " ++ var ++ " = phi " ++ typ ++ " " ++ formatPhiArgs updatedArgs
          return updatedPhi
        Left _ -> do
          label <- getCurrentLabel
          varVersions <- collectVariableVersions label ""
          case parse (lineParser varVersions) "" line of
            Right updatedLine -> return updatedLine
            Left _ -> return line
  where
    resolvePhiArg :: String -> (String, String) -> CodeGen (String, String)
    resolvePhiArg var (operand, label) = do
      varVersion <- getVariableVersion label (removeVersion operand)
      case varVersion of
        Just currentVersion -> return (currentVersion, label)
        Nothing -> do
          resolvedVersion <- findVersionInPredecessors label operand
          return (fromMaybe operand resolvedVersion, label)

findVersionInPredecessors :: String -> String -> CodeGen (Maybe String)
findVersionInPredecessors currentLabel var = do
  preds <- getPredecessors currentLabel
  findInLabels preds var

findInLabels :: [String] -> String -> CodeGen (Maybe String)
findInLabels [] _ = return Nothing
findInLabels (label:rest) var = do
  varVersion <- getVariableVersion label (removeVersion var)
  case varVersion of
    Just version -> return (Just version)
    Nothing -> findInLabels rest var

lineParser :: Map.Map String [String] -> Parser String
lineParser varVersions = do
    tokens <- many (variableParser varVersions <|> nonVariable)
    return $ concat tokens

variableParser :: Map.Map String [String] -> Parser String
variableParser varVersions = do
    char '%'
    name <- many1 (alphaNum <|> char '_' <|> char '.')
    let updated = case Map.lookup (removeVersion name) varVersions of
                    Just versions -> 
                        if name `elem` versions then '%' : name
                        else '%' : head versions
                    Nothing         -> "%error"
    return updated

removeVersion :: String -> String
removeVersion s = takeWhile (/= '.') s

phiParser :: Parser (String, String, [(String, String)])
phiParser = do
    many1 space
    var <- many1 (noneOf " ")
    string " = phi "
    typ <- many1 (noneOf " ")
    spaces
    args <- sepBy pairParser (string ", ")
    many anyToken
    return (var, typ, args)

pairParser :: Parser (String, String)
pairParser = do
    spaces
    string "[%"
    operand <- many1 (noneOf " ,]")
    spaces
    string ", %"
    label <- many1 (noneOf " ]")
    string "]"
    return (operand, label)

formatPhiArgs :: [(String, String)] -> String
formatPhiArgs args = intercalate ", " $ map (\(operand, label) -> "[%" ++ operand ++ ", %" ++ label ++ "]") args

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
  mapM_ processStmt stmts
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
  generateExprCode expr
  removeLastAssignment

processStmt (CondElse _ cond trueStmt falseStmt) = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel

  let trueLabel' = trueLabel ++ "_cond_true"
      falseLabel' = falseLabel ++ "_cond_else"
      endLabel' = endLabel ++ "_cond_end"
      doesTrueContainReturn = containsReturn trueStmt
      doesFalseContainReturn = containsReturn falseStmt
      

  currentLabel <- getCurrentLabel
  addEdge currentLabel trueLabel'
  addEdge currentLabel falseLabel'
  unless doesTrueContainReturn $ addEdge trueLabel' endLabel'
  unless doesFalseContainReturn $ addEdge falseLabel' endLabel'

  genCond cond trueLabel' falseLabel'

  emit $ trueLabel' ++ ":"
  updateCurrentLabel trueLabel'
  processStmt trueStmt
  unless doesTrueContainReturn $ emit $ "  br label %" ++ endLabel'


  emit $ falseLabel' ++ ":"
  updateCurrentLabel falseLabel'
  processStmt falseStmt
  unless doesFalseContainReturn $ emit $ "  br label %" ++ endLabel'

  emit $ endLabel' ++ ":"
  updateCurrentLabel endLabel'

processStmt (Cond _ cond stmt) = do
  trueLabel <- freshLabel
  endLabel <- freshLabel

  let trueLabel' = trueLabel ++ "_cond_true"
      endLabel' = endLabel ++ "_cond_end"
      doesStmtContainReturn = containsReturn stmt

  currentLabel <- getCurrentLabel
  addEdge currentLabel trueLabel'
  addEdge currentLabel endLabel'
  unless doesStmtContainReturn $ addEdge trueLabel' endLabel'

  genCond cond trueLabel' endLabel'
  emit $ trueLabel' ++ ":"
  updateCurrentLabel trueLabel'

  processStmt stmt
  unless doesStmtContainReturn $ emit $ "  br label %" ++ endLabel'
  
  emit $ endLabel' ++ ":"
  updateCurrentLabel endLabel'

processStmt (While _ cond stmt) = do
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  endLabel <- freshLabel

  let condLabel' = condLabel ++ "_while_cond"
      bodyLabel' = bodyLabel ++ "_while_body"
      endLabel' = endLabel ++ "_while_end"
      doesBodyContainReturn = containsReturn stmt

  currentLabel <- getCurrentLabel
  addEdge currentLabel condLabel'
  addEdge condLabel' endLabel'
  addEdge condLabel' bodyLabel'
  unless doesBodyContainReturn $ addEdge bodyLabel' condLabel'

  emit $ "  br label %" ++ condLabel'
  emit $ bodyLabel' ++ ":"
  updateCurrentLabel bodyLabel'
  processStmt stmt

  unless doesBodyContainReturn $ emit $ "  br label %" ++ condLabel'
  emit $ condLabel' ++ ":"
  updateCurrentLabel condLabel'
  genCond cond bodyLabel' endLabel'

  emit $ endLabel' ++ ":"
  updateCurrentLabel endLabel'

processStmt _ = return ()

containsReturn :: Stmt -> Bool
containsReturn stmt = case stmt of
  Ret _ _                       -> True
  VRet _                        -> True
  BStmt _ (Block _ stmts)       -> any containsReturn stmts
  CondElse _ _ t f              -> containsReturn t || containsReturn f
  Cond _ _ t                    -> containsReturn t
  While _ _ body                -> containsReturn body 
  _                             -> False

removeLastAssignment :: CodeGen ()
removeLastAssignment = do
  accCode <- getAccCode
  case accCode of
    (latestLine:rest) ->
      case break (== '=') latestLine of
        (_, '=':rhs) -> setAccCode ((" " ++ rhs) : rest)
        _            -> return ()
    [] -> return ()

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
  let functionName = getIdentName ident
  functionType <- getFunctionType functionName
  argTypes <- mapM checkExprType args
  
  let argsWithTypes = zip argsCode argTypes
      argsWithTypesStr = map (\(code, typ) -> typ ++ " " ++ code) argsWithTypes
      rhsCall = functionType ++ " @" ++ functionName ++ "(" ++ intercalate ", " argsWithTypesStr ++ ")"
  
  if functionType /= "void" then do
    tempVar <- freshTemp
    emit $ "  " ++ tempVar ++ " = call " ++ rhsCall
    return tempVar
  else do
    emit $ "  call " ++ rhsCall
    return ""

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
generateExprCode (EAnd _ expr1 expr2) = do      -- zbędna zmienna tymczasowa? likwidacja w optymalizacji LCS
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  tempVar <- freshTemp

  genCond (EAnd Nothing expr1 expr2) trueLabel falseLabel

  emit $ trueLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = 1"
  emit $ "  br label %" ++ endLabel

  emit $ falseLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = 0"
  emit $ "  br label %" ++ endLabel

  emit $ endLabel ++ ":"
  return tempVar
generateExprCode (EOr _ expr1 expr2) = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  tempVar <- freshTemp

  genCond (EOr Nothing expr1 expr2) trueLabel falseLabel

  emit $ trueLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = 1"
  emit $ "  br label %" ++ endLabel

  emit $ falseLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = 0"
  emit $ "  br label %" ++ endLabel

  emit $ endLabel ++ ":"
  return tempVar

genBinOp :: String -> Expr -> Expr -> CodeGen String    -- zmienna tymczasowa, do eliminacji w LCS
genBinOp llvmOp expr1 expr2 = do
  lhsCode <- generateExprCode expr1
  rhsCode <- generateExprCode expr2
  tempVar <- freshTemp
  let instr = "  " ++ tempVar ++ " = " ++ llvmOp ++ " " ++ lhsCode ++ ", " ++ rhsCode
  emit instr
  return tempVar

genCond :: Expr -> String -> String -> CodeGen ()
genCond (EAnd _ expr1 expr2) lTrue lFalse = do
  midLabel <- freshLabel
  genCond expr1 midLabel lFalse
  emit $ midLabel ++ ":"
  genCond expr2 lTrue lFalse
genCond (EOr _ expr1 expr2) lTrue lFalse = do
  midLabel <- freshLabel
  genCond expr1 lTrue midLabel
  emit $ midLabel ++ ":"
  genCond expr2 lTrue lFalse
genCond (ERel _ expr1 op expr2) lTrue lFalse = do
  lhsCode <- generateExprCode expr1
  rhsCode <- generateExprCode expr2
  lhsType <- checkExprType expr1
  let llvmOp = case (lhsType, op) of
        ("i32", LTH _) -> "icmp slt"
        ("i32", LE  _) -> "icmp sle"
        ("i32", GTH _) -> "icmp sgt"
        ("i32", GE  _) -> "icmp sge"
        ("i32", EQU _) -> "icmp eq"
        ("i32", NE  _) -> "icmp ne"
        _ -> error ("Unsupported relational operation for type: " ++ lhsType)
  tempVar <- freshTemp
  emit $ "  " ++ tempVar ++ " = " ++ llvmOp ++ " i32 " ++ lhsCode ++ ", " ++ rhsCode
  emit $ "  br i1 " ++ tempVar ++ ", label %" ++ lTrue ++ ", label %" ++ lFalse
genCond (Not _ expr) lTrue lFalse = genCond expr lFalse lTrue
genCond (EVar _ ident) lTrue lFalse = do
  let varName = "%" ++ getIdentName ident
  emit $ "  br i1 " ++ varName ++ ", label %" ++ lTrue ++ ", label %" ++ lFalse
genCond (ELitTrue _) lTrue _ = emit $ "  br label %" ++ lTrue
genCond (ELitFalse _) _ lFalse = emit $ "  br label %" ++ lFalse
genCond _ _ _ = error "Unsupported condition expression in genCond"

checkExprType :: Expr -> CodeGen String
checkExprType (ELitInt {}) = return "i32"
checkExprType (EString {}) = return "i8*"
checkExprType (EVar _ ident) = do
  getLocalVarsType (getIdentName ident)
checkExprType (EApp _ ident _) = do
  getFunctionType (getIdentName ident)
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
