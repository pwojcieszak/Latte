{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf, isInfixOf, find, nub)
import Data.Functor ((<$>))
import Data.Char (isSpace, isAlphaNum)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, noneOf, spaces, char, string, between, sepBy, space, many, anyToken, skipMany, alphaNum, (<|>), try)
import Data.Maybe (fromMaybe, mapMaybe)


import Control.Monad.State
import qualified Data.Set as Set
import GHC.RTS.Flags (CCFlags(doCostCentres))

type Err = Either String

type Label = String
type Temp = Int
type Addr = String
type LLVMType = String
type Name = String
type FlowGraph = Map.Map Label [Label]
type LocalsInfo = (Map.Map Name String, Map.Map Name String)

data CodeGenState = CodeGenState
  { nextTemp      :: Int
  , nextLabel     :: Int
  , nextGlobal    :: Int
  , nextRedeclaration    :: Int
  , localVars     :: Map.Map Name Addr        -- zmienna lokalna -> %adres 
  , localVarsTypes    :: Map.Map Name LLVMType    -- zmienna lokalna -> typ
  , varsInBlocks  :: Map.Map Label [Name]
  , functionTypes :: Map.Map Name LLVMType
  , codeAcc       :: [String]
  , flowGraph :: FlowGraph
  , currentLabel  :: String
  , variableVersions :: Map.Map Label (Map.Map String [String])          -- label var %varVersion
  , stringDecls :: [String]                     --- deklaracje globalne @string
  , stringMap :: Map.Map String Addr          -- "string" -> adres globalny @
  } deriving (Show)

initialState :: CodeGenState
initialState = CodeGenState
  { nextTemp = 0
  , nextLabel = 0
  , nextGlobal = 0
  , nextRedeclaration = 0
  , localVars = Map.empty
  , localVarsTypes = Map.empty
  , varsInBlocks = Map.empty
  , functionTypes = Map.fromList [("printInt", "void"), ("printString", "void"), ("error", "void"), ("readInt", "i32"), ("readString", "i8*")]
  , codeAcc = []
  , flowGraph = Map.empty
  , currentLabel  = ""
  , variableVersions = Map.empty
  , stringDecls = []
  , stringMap = Map.empty
  }

type CodeGen = State CodeGenState

freshTemp :: CodeGen Addr
freshTemp = do
  state <- get
  let temp = nextTemp state
  put state { nextTemp = temp + 1 }
  return $ "%t" ++ show temp

freshLabel :: CodeGen Label
freshLabel = do
  state <- get
  let lbl = nextLabel state
  put state { nextLabel = lbl + 1 }
  return $ "L" ++ show lbl

freshGlobal :: String -> CodeGen Addr
freshGlobal prefix = do
  state <- get
  let glb = nextGlobal state
  put state { nextGlobal = glb + 1 }
  return $ "@" ++ prefix ++ show glb

newIndex :: CodeGen Int
newIndex = do
  state <- get
  let version = nextRedeclaration state
  put state { nextRedeclaration = version + 1 }
  return version

addLocal :: Name -> Addr -> CodeGen (Addr)
addLocal name addr = do
  state <- get
  let locals = localVars state
  case Map.lookup name locals of
    Just existingAddr -> do
      index <- newIndex
      varsTypes <- gets localVarsTypes
      let newAddr = addr ++ "_" ++ show index
      let newLocal = tail newAddr
      let updatedMap = Map.insert name newAddr locals
      put state { localVars = Map.insert newLocal newAddr updatedMap }
      return newAddr
    Nothing -> do
      put state { localVars = Map.insert name addr locals }
      return addr

getLocal :: Name -> CodeGen Addr
getLocal name = do
  state <- get
  case Map.lookup name (localVars state) of 
    Just resolvedName -> return resolvedName
    Nothing -> return "getLocal error"

putLocals :: Map.Map Name Addr -> CodeGen ()
putLocals locals = do
  state <- get
  put state { localVars = locals }

addLocalVarsType :: Name -> LLVMType -> CodeGen ()
addLocalVarsType name addr = do
  state <- get
  let locals = localVarsTypes state
  put state { localVarsTypes = Map.insert name addr locals }

getLocalVarsType :: Name -> CodeGen LLVMType
getLocalVarsType name = do
  state <- get
  case Map.lookup name (localVarsTypes state) of
    Just localVarType -> return localVarType
    Nothing -> error $ "Local var type not found for: " ++ name

putLocalVarTypes :: Map.Map Name LLVMType -> CodeGen ()
putLocalVarTypes locals = do
  state <- get
  put state { localVars = locals }

getLocalVarTypes :: CodeGen (Map.Map Name LLVMType)
getLocalVarTypes = gets localVars

getLocalsInfo :: CodeGen LocalsInfo
getLocalsInfo = do
  locals <- gets localVars
  localsTypes <- getLocalVarTypes
  return (locals, localsTypes)

putLocalsInfo :: LocalsInfo -> CodeGen ()
putLocalsInfo (locals, localsTypes) = do
  putLocals locals
  putLocalVarTypes localsTypes

updateVarsInBlock :: CodeGen ()
updateVarsInBlock = do 
  state <- get
  stateVars <- gets varsInBlocks
  locals <- gets localVars
  let localNames = Map.keys locals
  currLabel <- getCurrentLabel
  put state { varsInBlocks = Map.insert currLabel localNames stateVars }
  return ()

getVarsInBlock :: Label -> CodeGen [Name]
getVarsInBlock label = do 
  state <- get
  blocksVars <- gets varsInBlocks
  case Map.lookup label blocksVars of
    Just vars -> return vars
    Nothing -> return []

getVarsInAllBlocks :: CodeGen (Map.Map Label [Name])
getVarsInAllBlocks = gets varsInBlocks

isVarInBlock :: Label -> Name -> CodeGen Bool
isVarInBlock label varName = do 
  blocksVars <- gets varsInBlocks
  case Map.lookup label blocksVars of
    Just vars -> return $ varName `elem` vars
    Nothing -> return False

addFunctionType :: Name -> String -> CodeGen ()
addFunctionType name ftype = do
  state <- get
  let functions = functionTypes state
  put state { functionTypes = Map.insert name ftype functions }

getFunctionType :: Name -> CodeGen LLVMType
getFunctionType name = do
  state <- get
  case Map.lookup name (functionTypes state) of
    Just functionType -> return functionType
    Nothing -> error $ "Function type not found for: " ++ name

addStringToMap :: String -> Addr -> CodeGen ()
addStringToMap value addr = do
  state <- get
  let strings = stringMap state
  put state { stringMap = Map.insert value addr strings }

getStringAddr :: String -> CodeGen (Maybe Addr)
getStringAddr value = do
  state <- get
  return $ Map.lookup value (stringMap state)

getStringValue :: Addr -> CodeGen String
getStringValue globalAddress = do
  state <- get
  let strMap = stringMap state
  case fst <$> find ((== globalAddress) . snd) (Map.toList strMap) of
    Just value -> return value
    Nothing    -> return "error getStringValue" 

clearStateAtStartOfFun :: CodeGen ()
clearStateAtStartOfFun = do 
  funTypes <- gets functionTypes
  nextGlb <- gets nextGlobal
  strDecls <- gets stringDecls
  stringAddr <- gets stringMap
  put initialState {functionTypes = funTypes, nextGlobal = nextGlb, stringDecls = strDecls, stringMap = stringAddr}

emit :: String -> CodeGen ()
emit instr = modify (\s -> s { codeAcc = instr : codeAcc s })

emitString :: String -> CodeGen ()
emitString instr = modify (\s -> s { stringDecls = instr : stringDecls s })

getAccCode :: CodeGen [String]
getAccCode = do
  state <- get
  let code = codeAcc state
  return code

setAccCode :: [String] -> CodeGen ()
setAccCode code = do
  state <- get
  put state { codeAcc = code }

getStringDecls :: CodeGen [String]
getStringDecls = do
  state <- get
  let strings = "" : stringDecls state
  return (reverse strings)

addEdge :: Label -> Label -> CodeGen ()
addEdge from to = do
  state <- get
  let graph = flowGraph state
  let updatedGraph = Map.insertWith (++) from [to] graph
  put state { flowGraph = updatedGraph }

getPredecessors :: Label -> CodeGen [Label]
getPredecessors label = do
  flowGraph <- gets flowGraph
  let predecessors = [predLabel | (predLabel, successors) <- Map.toList flowGraph, label `elem` successors]
  return predecessors

updateCurrentLabel :: Label -> CodeGen ()
updateCurrentLabel newLabel = do
  state <- get
  put state { currentLabel = newLabel }

getCurrentLabel :: CodeGen Label
getCurrentLabel = do
  state <- get
  return (currentLabel state)

getVariableVersion :: Label -> Name -> CodeGen (Maybe Addr)
getVariableVersion label var = do
  state <- get
  let versions = variableVersions state
  return $ do
    varMap <- Map.lookup label versions
    versionList <- Map.lookup var varMap
    return $ head versionList

getVariableVersionsByLabel :: Label -> CodeGen (Map.Map Name [Addr])
getVariableVersionsByLabel label = do
  state <- get
  let versions = variableVersions state
  case Map.lookup label versions of
    Just map -> return map
    Nothing -> return Map.empty

collectAllVariableVersions :: Label -> Label -> Set.Set Label -> CodeGen (Map.Map Name [Addr])
collectAllVariableVersions label startLabel visited = do
  state <- get
  let versions = variableVersions state
  let currentVars = Map.findWithDefault Map.empty label versions

  preds <- getPredecessors label
  
  if null preds || Set.member label visited then
    return currentVars
  else do
    let visited' = Set.insert label visited

    predVarsMerged <- collectPredecessorVariables preds visited'

    return $ Map.unionWith (++) currentVars predVarsMerged

collectPredecessorVariables :: [Label] -> Set.Set Label -> CodeGen (Map.Map Name [Addr])
collectPredecessorVariables preds visited = do
  predVars <- forM preds $ \predLabel -> do
    collectAllVariableVersions predLabel predLabel visited
  return $ Map.unionsWith (++) predVars

collectPredecessorVariablesForLabel :: Label -> CodeGen (Map.Map Name [Addr])
collectPredecessorVariablesForLabel label = do
  preds <- getPredecessors label
  collectPredecessorVariables preds Set.empty

updateVariableVersion :: Label -> Name -> Addr -> CodeGen ()
updateVariableVersion label var version = do
  state <- get
  let versions = variableVersions state
      updatedLabelMap = case Map.lookup label versions of
        Just varMap -> Map.insertWith (++) var [version] varMap
        Nothing     -> Map.singleton var [version]
      updatedVersions = Map.insert label updatedLabelMap versions
  put state { variableVersions = updatedVersions }

generatePredefinedFunctions :: [String]
generatePredefinedFunctions =
  [ "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @readString()",
    "declare i8* @concat(i8*, i8*)\n"
  ]

runCodeGen :: CodeGenState -> CodeGen a -> (a, CodeGenState)
runCodeGen initialState codeGen = runState codeGen initialState

generateLLVM :: Program -> CodeGen String
generateLLVM program = do
  let predefinedCode = generatePredefinedFunctions
  finalCode <- generateProgramCode program
  strings <- getStringDecls
  return $ unlines (predefinedCode ++ strings ++ finalCode)

generateProgramCode :: Program -> CodeGen [String]
generateProgramCode (Program _ topDefs) = do
  mapM_ collectFunctionTypes topDefs
  functionCodes <- mapM generateFunction topDefs
  return $ concat functionCodes

collectFunctionTypes :: TopDef -> CodeGen ()
collectFunctionTypes (FnDef _ returnType ident args block) = do
  let llvmReturnType = getLLVMType returnType
      functionName = getIdentName ident
  addFunctionType functionName llvmReturnType
    
generateFunction :: TopDef -> CodeGen [String]
generateFunction (FnDef _ returnType ident args block) = do
  let llvmReturnType = getLLVMType returnType
      functionName = getIdentName ident
  clearStateAtStartOfFun

  llvmArgs <- foldM generateFunctionArg [] args
  let header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"

  entryLabel <- freshLabel
  addArgsToLabelVarMap entryLabel args    -- przyda się do na początku SSA
  emit $ entryLabel ++ ":"
  updateCurrentLabel entryLabel

  generateBlockCode block
  updateVarsInBlock
  blockCode <- getAccCode                                                               -- Odwrócony kod ciała funkcji bez SSA i Phi

  codeWithReturns <- addReturnIfNeeded blockCode returnType                             -- Front akceptuje funkcje void bez return i funkcje innego typu jeśli np. ConElse ma dwa returny. LLVM tego nie akceptuje.
  blockCodeWithSimplePhi <- processLabelsForPhi ("}\n" : codeWithReturns)               -- Wstawiam phi dla wszystkich zmiennych w blokach z kilkoma poprzednikami 
  blockWithSimplePhiAndSSA <- renameToSSA blockCodeWithSimplePhi                        -- Wprowadzam SSA i uzupelniam informacje o zmiennych i wersjach w blokach
  updatedCode <- updateVariables blockWithSimplePhiAndSSA                               -- Przemianowuję zmienne analizując przepływ bloków
  let processedAssCode = processAssignments updatedCode                                 -- Pozbywam się sztucznych "%x = 2"
  return $ header : processedAssCode

getIdentName :: Ident -> String
getIdentName (Ident name) = name

getLLVMType :: Type -> String
getLLVMType (Int _) = "i32"
getLLVMType (Str _) = "i8*"
getLLVMType (Bool _) ="i1"
getLLVMType (Void _) = "void"
getLLVMType (Fun _ returnType paramTypes) = getLLVMType returnType

generateFunctionArg :: [String] -> Arg -> CodeGen [String]
generateFunctionArg argsCode (Arg _ argType ident) = do
  let llvmType = getLLVMType argType
      argName = getIdentName ident
  localName <- addLocal argName ("%" ++ argName)
  addLocalVarsType argName llvmType
  return (argsCode ++ [llvmType ++ " " ++ localName])

addArgsToLabelVarMap :: String -> [Arg] -> CodeGen ()
addArgsToLabelVarMap label args = do
  forM_ args $ \(Arg _ _ ident) -> do
    let argName = getIdentName ident
    updateVariableVersion label argName ('%':argName)

addReturnIfNeeded :: [String] -> Type -> CodeGen [String]
addReturnIfNeeded blockCode returnType = do 
  let llvmReturnType = getLLVMType returnType
  defaultForType <- getDefaultForType returnType
  
  let defaultReturnInstruction = if llvmReturnType == "void"
                          then "  ret void"
                          else "  ret " ++ llvmReturnType ++ " " ++ defaultForType

  if not (isRetInstruction defaultReturnInstruction (head blockCode))
    then return $ defaultReturnInstruction : blockCode 
    else return blockCode

isRetInstruction :: String -> String -> Bool
isRetInstruction returnInstruction line = "ret" `elem` words line

generateBlockCode :: Block -> CodeGen ()
generateBlockCode (Block _ stmts) = do
  mapM_ processStmt stmts

processStmt :: Stmt -> CodeGen ()
processStmt (Decl _ varType items) = do
  mapM_ (generateVarDecl varType) items
processStmt (BStmt _ (Block _ stmts)) = do
  updateVarsInBlock
  preBlockLocalsInfo <- getLocalsInfo
  currLabel <- getCurrentLabel
  startBlockLabel <- freshLabel
  endBlockLabel <- freshLabel

  addEdge currLabel startBlockLabel
  addEdge currLabel endBlockLabel

  emit $ "  br label %" ++ startBlockLabel
  updateCurrentLabel startBlockLabel
  emit $ startBlockLabel ++ ":"
  mapM_ processStmt stmts
  updateVarsInBlock
  emit $ "  br label %" ++ endBlockLabel

  updateCurrentLabel endBlockLabel
  emit $ endBlockLabel ++ ":"
  putLocalsInfo preBlockLocalsInfo
processStmt (Ass _ ident expr) = do
  exprCode <- generateExprCode expr
  localName <- getLocal (getIdentName ident)
  let assignCode = "  " ++ localName ++ " = " ++ exprCode
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
  preBlockLocalsInfo <- getLocalsInfo
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  let trueStmts = extractStmts trueStmt
  let falseStmts = extractStmts falseStmt

  let trueLabel' = trueLabel ++ "_cond_true"
      falseLabel' = falseLabel ++ "_cond_else"
      endLabel' = endLabel ++ "_cond_end"
      doesTrueContainReturn = any containsReturn trueStmts
      doesFalseContainReturn = any containsReturn falseStmts
      

  currentLabel <- getCurrentLabel
  addEdge currentLabel trueLabel'
  addEdge currentLabel falseLabel'
  unless doesTrueContainReturn $ addEdge trueLabel' endLabel'
  unless doesFalseContainReturn $ addEdge falseLabel' endLabel'

  genCond cond trueLabel' falseLabel'

  emit $ trueLabel' ++ ":"
  updateVarsInBlock
  updateCurrentLabel trueLabel'
  mapM_ processStmt trueStmts
  unless doesTrueContainReturn $ emit $ "  br label %" ++ endLabel'
  updateVarsInBlock
  putLocalsInfo preBlockLocalsInfo
  
  emit $ falseLabel' ++ ":"
  updateCurrentLabel falseLabel'
  mapM_ processStmt falseStmts
  unless doesFalseContainReturn $ emit $ "  br label %" ++ endLabel'
  updateVarsInBlock
  putLocalsInfo preBlockLocalsInfo

  unless (doesTrueContainReturn && doesFalseContainReturn) $ do
    emit $ endLabel' ++ ":"
    updateCurrentLabel endLabel'

processStmt (Cond _ cond stmt) = do
  preBlockLocalsInfo <- getLocalsInfo
  trueLabel <- freshLabel
  endLabel <- freshLabel
  let stmts = extractStmts stmt

  let trueLabel' = trueLabel ++ "_cond_true"
      endLabel' = endLabel ++ "_cond_end"
      doesStmtContainReturn = any containsReturn stmts

  currentLabel <- getCurrentLabel
  addEdge currentLabel trueLabel'
  unless doesStmtContainReturn $ addEdge trueLabel' endLabel'

  genCond cond trueLabel' endLabel'
  emit $ trueLabel' ++ ":"
  updateVarsInBlock
  updateCurrentLabel trueLabel'

  mapM_ processStmt stmts

  unless doesStmtContainReturn $ emit $ "  br label %" ++ endLabel'
  emit $ endLabel' ++ ":"
  updateVarsInBlock
  updateCurrentLabel endLabel'
  putLocalsInfo preBlockLocalsInfo

processStmt (While _ cond stmt) = do
  preBlockLocalsInfo <- getLocalsInfo
  condLabel <- freshLabel
  bodyLabel <- freshLabel
  endLabel <- freshLabel

  let stmts = extractStmts stmt

  let condLabel' = condLabel ++ "_while_cond"
      bodyLabel' = bodyLabel ++ "_while_body"
      endLabel' = endLabel ++ "_while_end"
      doesBodyContainReturn = any containsReturn stmts

  currentLabel <- getCurrentLabel
  addEdge currentLabel condLabel'
  addEdge condLabel' endLabel'
  addEdge condLabel' bodyLabel'
  unless doesBodyContainReturn $ addEdge bodyLabel' condLabel'

  emit $ "  br label %" ++ condLabel'
  emit $ bodyLabel' ++ ":"
  updateVarsInBlock
  updateCurrentLabel bodyLabel'
  mapM_ processStmt stmts

  updateVarsInBlock
  putLocalsInfo preBlockLocalsInfo

  unless doesBodyContainReturn $ emit $ "  br label %" ++ condLabel'
  emit $ condLabel' ++ ":"
  updateCurrentLabel condLabel'
  genCond cond bodyLabel' endLabel'

  updateVarsInBlock
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

generateVarDecl :: Type -> Item -> CodeGen ()
generateVarDecl varType (NoInit _ ident) = do
  defaultValue <- getDefaultForType varType
  generateVarDeclString varType ident defaultValue

generateVarDecl varType (Init _ ident expr) = do
  exprCode <- generateExprCode expr
  generateVarDeclString varType ident exprCode
  

generateVarDeclString :: Type -> Ident -> String -> CodeGen ()
generateVarDeclString varType ident value = do 
  let variableName = "%" ++ getIdentName ident
      llvmType = getLLVMType varType
  localName <- addLocal (getIdentName ident) variableName
  addLocalVarsType (tail localName) llvmType
  emit $ "  " ++ localName ++ " = " ++ value

removeLastAssignment :: CodeGen ()
removeLastAssignment = do
  accCode <- getAccCode
  case accCode of
    (latestLine:rest) ->
      case break (== '=') latestLine of
        (_, '=':rhs) -> setAccCode ((" " ++ rhs) : rest)
        _            -> return ()
    [] -> return ()

extractStmts :: Stmt -> [Stmt]
extractStmts stmt = case stmt of
  BStmt _ (Block _ stmts) -> stmts
  statement -> [statement] 

assignString :: String -> CodeGen String      -- tworzy lub zwraca istniejacy global dla string
assignString value = do
  state <- get
  let escapedValue = convertString value
      strAdr = Map.lookup escapedValue (stringMap state)
  
  case strAdr of
    Just address -> return address
    Nothing -> do
      newGlobal <- freshGlobal "s"
      let strLength = length escapedValue + 1 
          llvmDecl = newGlobal ++ " = private constant [" ++ show strLength ++ " x i8] c\"" ++ escapedValue ++ "\\00\""
      emitString llvmDecl
      addStringToMap escapedValue newGlobal
      return newGlobal


generateExprCode :: Expr -> CodeGen String
generateExprCode (ELitInt _ value) = return $ show value
generateExprCode (EString _ value) = do
  globalAddress <- assignString (convertString value)
  tempVar <- freshTemp
  let strLength = length value + 1
      code = "  " ++ tempVar ++ " = bitcast [" ++ show strLength ++ " x i8]* " ++ globalAddress ++ " to i8*"
  emit code
  return tempVar
generateExprCode (EVar _ ident) = do                                                -- zwraca lokalne zmienne (adresy zmiennych) (dla string też)
  getLocal (getIdentName ident)
generateExprCode (ELitTrue _) = return "1"
generateExprCode (ELitFalse _) = return "0"
generateExprCode (EApp _ ident args) = do
  let functionName = getIdentName ident
  functionType <- getFunctionType functionName
  argsCode <- mapM generateExprCode args
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
  tempVar <- freshTemp
  emit $ "  " ++ tempVar ++ " = sub i32 0, " ++ exprCode
  return tempVar
generateExprCode (Not _ expr) = do
  exprCode <- generateExprCode expr
  tempVar <- freshTemp
  emit $ "  " ++ tempVar ++ " = xor i1 " ++ exprCode ++ ", 1"
  return tempVar
generateExprCode (EMul _ expr1 op expr2) = do
  let llvmOp = case op of
        Times _ -> "mul i32"
        Div _   -> "sdiv i32"
        Mod _   -> "srem i32"
  genBinOp llvmOp expr1 expr2
generateExprCode (EAdd _ expr1 op expr2) = do
  lhsType <- checkExprType expr1
  if lhsType == "i8*" then
    concatStrings expr1 expr2
  else do
    let llvmOp = case op of
          Plus _  -> "add " ++ lhsType
          Minus _ -> "sub i32"
    genBinOp llvmOp expr1 expr2
generateExprCode (ERel _ expr1 op expr2) = do
  lhsType <- checkExprType expr1
  rhsType <- checkExprType expr2
  let llvmOp = case (lhsType, rhsType, op) of
        ("i32", "i32", LTH _) -> "icmp slt " ++ lhsType
        ("i32", "i32", LE  _) -> "icmp sle " ++ lhsType
        ("i32", "i32", GTH _) -> "icmp sgt " ++ lhsType
        ("i32", "i32", GE  _) -> "icmp sge " ++ lhsType
        ("i32", "i32", EQU _) -> "icmp eq " ++ lhsType
        ("i32", "i32", NE  _) -> "icmp ne " ++ lhsType
        ("i1", "i1", EQU _) -> "icmp eq " ++ lhsType
        ("i1", "i1", NE  _) -> "icmp ne " ++ lhsType
        ("i8*", "i8*", EQU _) -> "icmp eq " ++ lhsType
        ("i8*", "i8*", NE  _) -> "icmp ne " ++ lhsType
        ("i1", "i32", NE  _) -> "icmp ne " ++ lhsType
        ("i32", "i1", NE  _) -> "icmp ne " ++ rhsType
        ("i1", "i32", EQU  _) -> "icmp eq " ++ lhsType
        ("i32", "i1", EQU  _) -> "icmp eq " ++ rhsType
        _ -> error ("Unsupported relational operation for type: " ++ lhsType)
  genBinOp llvmOp expr1 expr2
generateExprCode (EAnd _ expr1 expr2) = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  tempVar <- freshTemp
  addEdge trueLabel endLabel
  addEdge falseLabel endLabel

  genCond (EAnd Nothing expr1 expr2) trueLabel falseLabel
  emitRelExpLabels tempVar trueLabel falseLabel endLabel 

  return tempVar
generateExprCode (EOr _ expr1 expr2) = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  tempVar <- freshTemp
  addEdge trueLabel endLabel
  addEdge falseLabel endLabel

  genCond (EOr Nothing expr1 expr2) trueLabel falseLabel
  emitRelExpLabels tempVar trueLabel falseLabel endLabel 

  return tempVar

emitRelExpLabels :: Addr -> Label -> Label -> Label -> CodeGen ()
emitRelExpLabels tempVar trueLabel falseLabel endLabel = do
  emit $ trueLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 0"
  emit $ "  br label %" ++ endLabel

  emit $ falseLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 1"
  emit $ "  br label %" ++ endLabel

  emit $ endLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = phi i1 [" ++ tempVar ++ ", %" ++ trueLabel ++ "], [" ++ tempVar ++ ", %" ++ falseLabel ++ "]" 

genBinOp :: String -> Expr -> Expr -> CodeGen String
genBinOp llvmOp expr1 expr2 = do
  lhsCode <- generateExprCode expr1
  rhsCode <- generateExprCode expr2
  tempVar <- freshTemp
  let instr = "  " ++ tempVar ++ " = " ++ llvmOp ++ " " ++ lhsCode ++ ", " ++ rhsCode
  emit instr
  return tempVar

concatStrings :: Expr -> Expr -> CodeGen String
concatStrings expr1 expr2 = do
  lhsCode <- generateExprCode expr1
  rhsCode <- generateExprCode expr2
  tempVar <- freshTemp
  emit $ "  " ++ tempVar ++ " = call i8* @concat(i8* " ++ lhsCode ++ ", i8* " ++ rhsCode ++ ")"
  return tempVar

genCond :: Expr -> String -> String -> CodeGen ()
genCond (EAnd _ expr1 expr2) lTrue lFalse = do
  currLabel <- getCurrentLabel
  midLabel <- freshLabel

  updateVarsInBlock
  genCond expr1 midLabel lFalse
  updateCurrentLabel midLabel

  emit $ midLabel ++ ":"
  genCond expr2 lTrue lFalse
genCond (EOr _ expr1 expr2) lTrue lFalse = do
  currLabel <- getCurrentLabel
  midLabel <- freshLabel

  updateVarsInBlock
  genCond expr1 lTrue midLabel
  updateCurrentLabel midLabel

  emit $ midLabel ++ ":"
  genCond expr2 lTrue lFalse
genCond (ERel _ expr1 op expr2) lTrue lFalse = do
  lhsCode <- generateExprCode expr1
  rhsCode <- generateExprCode expr2
  lhsType <- checkExprType expr1
  rhsType <- checkExprType expr2
  let llvmOp = case (lhsType, rhsType, op) of
        ("i32", "i32", LTH _) -> "icmp slt " ++ lhsType
        ("i32", "i32", LE  _) -> "icmp sle " ++ lhsType
        ("i32", "i32", GTH _) -> "icmp sgt " ++ lhsType
        ("i32", "i32", GE  _) -> "icmp sge " ++ lhsType
        ("i32", "i32", EQU _) -> "icmp eq " ++ lhsType
        ("i32", "i32", NE  _) -> "icmp ne " ++ lhsType
        ("i1", "i1", EQU _) -> "icmp eq " ++ lhsType
        ("i1", "i1", NE  _) -> "icmp ne " ++ lhsType
        ("i8*", "i8*", EQU _) -> "icmp eq " ++ lhsType
        ("i8*", "i8*", NE  _) -> "icmp ne " ++ lhsType
        ("i1", "i32", NE  _) -> "icmp ne " ++ lhsType
        ("i32", "i1", NE  _) -> "icmp ne " ++ rhsType
        ("i1", "i32", EQU  _) -> "icmp eq " ++ lhsType
        ("i32", "i1", EQU  _) -> "icmp eq " ++ rhsType
        _ -> error ("Unsupported relational operation for type: " ++ lhsType)
  tempVar <- freshTemp
  emit $ "  " ++ tempVar ++ " = " ++ llvmOp ++ " " ++ lhsCode ++ ", " ++ rhsCode
  emit $ "  br i1 " ++ tempVar ++ ", label %" ++ lTrue ++ ", label %" ++ lFalse
genCond (Not _ expr) lTrue lFalse = genCond expr lFalse lTrue
genCond (EVar _ ident) lTrue lFalse = do
  let varName = "%" ++ getIdentName ident
  emit $ "  br i1 " ++ varName ++ ", label %" ++ lTrue ++ ", label %" ++ lFalse
genCond (ELitTrue _) lTrue _ = emit $ "  br label %" ++ lTrue
genCond (ELitFalse _) _ lFalse = emit $ "  br label %" ++ lFalse
genCond (EApp _ ident args) lTrue lFalse = do
  callResult <- generateExprCode (EApp Nothing ident args)
  tempVar <- freshTemp
  retType <- getFunctionType (getIdentName ident)
  emit $ "  " ++ tempVar ++ " = icmp ne " ++ retType ++ " " ++ callResult ++ ", 0"
  emit $ "  br i1 " ++ tempVar ++ ", label %" ++ lTrue ++ ", label %" ++ lFalse
genCond smth ltrue lfalse = error $ "Unsupported condition expression in genCond " ++ show smth ++ " " ++ ltrue ++ " " ++ lfalse

checkExprType :: Expr -> CodeGen String
checkExprType (ELitInt {}) = return "i32"
checkExprType (EString {}) = return "i8*"
checkExprType (EVar _ ident) = do
  localName <- getLocal (getIdentName ident)
  getLocalVarsType (tail localName)
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

getDefaultForType :: Type -> CodeGen String
getDefaultForType (Int _) = return "0"
getDefaultForType (Str _) = do 
  globalAddress <- assignString ""
  return $ "bitcast [1 x i8]* " ++ globalAddress ++ " to i8*"
getDefaultForType (Bool _) = return "false"
getDefaultForType _ = return ""

convertString :: String -> String
convertString = concatMap convertChar
  where
    convertChar c = case c of
      '\n' -> "\\0A"
      '\t' -> "\\09"
      '\r' -> "\\0D"
      '\\' -> "\\5C"
      '"'  -> "\\22" 
      '\0' -> "\\00" 
      c -> [c]

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
    varsInBlock <- getVarsInBlock label
    case predecessors of
      []     -> return []
      [_]    -> return []
      preds  -> mapM (generatePhiInstr preds) varsInBlock

generatePhiInstr :: [String] -> String -> CodeGen String
generatePhiInstr predecessors varName = do
  varType <- getLocalVarsType varName
  filteredPredecessors <- filterM (`isVarInBlock` varName) predecessors
  if length filteredPredecessors == length predecessors
    then return $ "  %" ++ varName ++ " = phi " ++ varType ++ " " ++ generatePhiSources filteredPredecessors varName
    else return ""

generatePhiSources :: [String] -> String -> String
generatePhiSources preds varName =
    intercalate ", " [ "[%" ++ varName ++ ", %" ++ pred ++ "]" | pred <- preds ]

renameToSSA :: [String] -> CodeGen [String]
renameToSSA codeLines = process codeLines Map.empty []
  where
    process [] _ acc = return (reverse acc)
    process (line:rest) varVersions acc                                 -- varVersions służy do poprawnego określania kolejnej wersji dla zmiennej (varName -> wersja)
      | Just (var, expr) <- parseAssignment line = do
          let newVersion = Map.findWithDefault (-1) var varVersions + 1
              newVarAddr = "%" ++ var ++ "." ++ show newVersion
              newLine = "  " ++ newVarAddr ++ " = " ++ renameVariables expr varVersions
              updatedVarVersions = Map.insert var newVersion varVersions
          currentLabel <- getCurrentLabel
          updateVariableVersion currentLabel var newVarAddr             -- dodaje informacje o zmiennych w bloku (var -> Addr)
          versions <- collectAllVariableVersions currentLabel currentLabel Set.empty
          process rest updatedVarVersions ((newLine) : acc)
      | last line == ':' = do 
          let labelName = init line
          updateCurrentLabel labelName
          process rest varVersions (line : acc)
      | otherwise = do
          let newLine = renameVariables line varVersions
          process rest varVersions (newLine : acc)

parseAssignment :: String -> Maybe (String, String)
parseAssignment line =
  let trimmedLine = trim line
  in case span (/= '=') trimmedLine of
       ('%':varName, '=':rest) ->
         let var = trim varName
             expr = trim rest
         in Just (var, expr)
       _ -> Nothing

renameVariables :: String -> Map.Map String Int -> String
renameVariables line varVersions =
    case parse (lineParserSSA varVersions) "" line of
        Left err  -> error $ "renameVariables error: " ++ show err
        Right res -> res

lineParserSSA :: Map.Map String Int -> Parser String
lineParserSSA varVersions = do
    tokens <- many (variableParserSSA varVersions <|> nonVariable)
    return $ concat tokens

variableParserSSA :: Map.Map String Int -> Parser String
variableParserSSA varVersions = do
    char '%'
    name <- many1 (alphaNum <|> char '_' <|> char '.')
    let updated = case Map.lookup name varVersions of
                    Just version -> "%" ++ name ++ "." ++ show version
                    Nothing      -> "%" ++ name
    return updated

nonVariable :: Parser String
nonVariable = many1 (noneOf "%")

updateVariables :: [String] -> CodeGen [String]
updateVariables code = do
  initialState <- collectPredecessorVariablesForLabel "L0" 
  (_, processedLines) <- foldM processLine (initialState, []) code
  return $ reverse processedLines

processLine :: (Map.Map String [String], [String]) -> String -> CodeGen (Map.Map String [String], [String])
processLine (versions, processedLines) line = do        -- versions a -> [%a.0]
  if not (null line) && last line == ':' then
    let label = init line in do
      updateCurrentLabel label
      versions <- collectPredecessorVariablesForLabel label
      return (versions, line : processedLines)
  else
    case parse phiParser "" line of
      Right (var, typ, args) -> do          -- %varVer, args=(%var, label??)
        updatedArgs <- mapM resolvePhiArg args
        let updatedVersions = Map.insertWith (++) (removeVersion (tail var)) [var] versions
        let updatedPhi = "  " ++ var ++ " = phi " ++ typ ++ " " ++ formatPhiArgs updatedArgs
        return (updatedVersions, updatedPhi : processedLines)
      Left _ -> do
        let wordsInLine = words line
        if "=" `elem` wordsInLine then
          let (lhs, rhs) = splitAssignment line
              varAddrVer = trim lhs
              updatedVersions = Map.insertWith (++) (removeVersion (tail varAddrVer)) [varAddrVer] versions
              assignment = lhs ++ "="
          in do
            case parse (lineParserCFG versions) "" rhs of
              Right updatedLine -> return (updatedVersions, (assignment ++ updatedLine) : processedLines)
              Left _ -> return (updatedVersions, (assignment ++ rhs) : processedLines)
        else
          case parse (lineParserCFG versions) "" line of
            Right updatedLine -> return (versions, updatedLine : processedLines)
            Left _ -> return (versions, line :  processedLines) 

removeVersion :: String -> String
removeVersion s = do
  if '.' `elem` s
    then reverse $ drop 1 $ dropWhile (/= '.') $ reverse s
    else s

phiParser :: Parser (String, String, [(String, String)])
phiParser = do
    many1 space
    varAddr <- many1 (noneOf " ")
    string " = phi "
    typ <- many1 (noneOf " ")
    spaces
    args <- sepBy pairParser (string ", ")
    many anyToken
    return (varAddr, typ, args)

pairParser :: Parser (String, String)
pairParser = do
    spaces
    string "["
    operandAddressWithVersion <- many1 (noneOf " ,]")
    spaces
    string ", %"
    label <- many1 (noneOf " ]")
    string "]"
    return (operandAddressWithVersion, label)

resolvePhiArg :: (String, String) -> CodeGen (String, String)
resolvePhiArg (operand, label) = do
  varVersions <- collectAllVariableVersions label label Set.empty
  let updated = case Map.lookup (removeVersion (tail operand)) varVersions of
                    Just versions -> head versions
                    Nothing -> "resolvePhiArg error"
  return (updated, label)

formatPhiArgs :: [(String, String)] -> String
formatPhiArgs args = intercalate ", " $ map (\(operand, label) -> "[" ++ operand ++ ", %" ++ label ++ "]") args

splitAssignment :: String -> (String, String)
splitAssignment line = 
  let (lhs, rest) = span (/= '=') line
      rhs = drop 1 rest
  in (lhs, rhs)

lineParserCFG :: Map.Map String [String] -> Parser String
lineParserCFG varVersions = do
    tokens <- many (variableParserCFG varVersions <|> nonVariable)
    return $ concat tokens

variableParserCFG :: Map.Map String [String] -> Parser String
variableParserCFG varVersions = do
    char '%'
    name <- many1 (alphaNum <|> char '_' <|> char '.')
    let updated = case Map.lookup (removeVersion name) varVersions of
                    Just versions -> head versions
                    Nothing  -> "%"++name      -- zmienna label
    return updated

processAssignments :: [String] -> [String]
processAssignments finalCode = processLines finalCode Map.empty

processLines :: [String] -> Map.Map String String -> [String]
processLines [] _ = []
processLines (line : rest) assignments                  -- assignments to mapa w której trzymam zmienne które chcę podmienić i ich wartości (%addr -> %addr | literal)
  | "=" `isInfixOf` line && length (words line) == 3
  = let
      (lhs, rhs) = extractAssignment line
      newAssignments = Map.insert lhs rhs assignments
    in processLines rest newAssignments

  -- | "phi" `isInfixOf` line
  -- = let
  --     updatedLine = replaceVars line assignments
  --   in case parse phiParser "" updatedLine of
  --   Right (var, typ, args) -> 
  --     let varBase = removeVersion var
  --         isInvalidArg (argVar, _) = argVar == var || removeVersion argVar == varBase
  --     in if allEqual args || all isInvalidArg args
  --       in if False
  --       then 
  --         let value = fst (head args)
  --             newAssignments = Map.insert var value assignments
  --         in processLines rest newAssignments
  --       else updatedLine : processLines rest assignments
  --   Left _ -> updatedLine : processLines rest assignments

  | otherwise
  = let updatedLine = replaceVars line assignments
    in updatedLine : processLines rest assignments

extractAssignment :: String -> (String, String)
extractAssignment line = 
  let parts = words line
      lhs = head parts
      rhs = last parts
  in (lhs, rhs)

replaceVars :: String -> Map.Map String String -> String
replaceVars line assignments =
    case parse (lineParserAss assignments) "" line of
        Left err  -> error $ "replaceVars: " ++ show err
        Right res -> res

lineParserAss :: Map.Map String String -> Parser String
lineParserAss assignments = do
    tokens <- many (variableParserAss assignments <|> nonVariable)
    return $ concat tokens

variableParserAss :: Map.Map String String -> Parser String
variableParserAss assignments = do
    char '%'
    varAddr <- many1 (alphaNum <|> char '_' <|> char '.')
    let updated = lookUpAssignment assignments ('%' : varAddr)
    return updated

lookUpAssignment :: Map.Map String String -> String -> String
lookUpAssignment assignments varAddr = do
  case Map.lookup varAddr assignments of
                      Just rhs ->  
                        if head rhs == '%' 
                          then lookUpAssignment assignments rhs
                          else rhs
                      Nothing  -> varAddr

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

mapToString :: Map.Map String [String] -> String
mapToString m =
    let entries = Map.toList m
        formatEntry (key, values) = key ++ " -> [" ++ intercalate ", " values ++ "]"
    in intercalate "\n" (map formatEntry entries)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs