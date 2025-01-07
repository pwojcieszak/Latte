{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map
import Data.List (intercalate, isPrefixOf, isInfixOf, find)
import Data.Functor ((<$>))
import Data.Char (isSpace, isAlphaNum)
import Text.Parsec.String (Parser)
import Text.Parsec (parse, many1, noneOf, spaces, char, string, between, sepBy, space, many, anyToken, skipMany, alphaNum, (<|>), try)
import Data.Maybe (fromMaybe)


import Control.Monad.State
import qualified Data.Set as Set

type Err = Either String

type Label = Int
type Temp = Int
type Global = Int
type Name = String
type FlowGraph = Map.Map String [String]
type LocalsInfo = (Map.Map Name String, Map.Map Name String)

data CodeGenState = CodeGenState
  { nextTemp      :: Temp
  , nextLabel     :: Label
  , nextGlobal    :: Int
  , nextRedeclaration    :: Int
  , localVars     :: Map.Map Name String        -- zmienna lokalna -> %adres 
  , localVarsTypes    :: Map.Map Name String    -- zmienna lokalna -> typ
  , functionTypes :: Map.Map Name String
  , codeAcc       :: [String]
  , flowGraph :: FlowGraph
  , currentLabel  :: String
  , variableVersions :: Map.Map String (Map.Map String [String])          -- label var %varVersion
  , stringDecls :: [String]                     --- deklaracje globalne @string
  , stringMap :: Map.Map String String          -- "string" -> adres globalny @
  } deriving (Show)

initialState :: CodeGenState
initialState = CodeGenState
  { nextTemp = 0
  , nextLabel = 0
  , nextGlobal = 0
  , nextRedeclaration = 0
  , localVars = Map.empty
  , localVarsTypes = Map.empty
  , functionTypes = Map.fromList [("printInt", "void"), ("printString", "void"), ("error", "void"), ("readInt", "i32"), ("readString", "i8*")]
  , codeAcc = []
  , flowGraph = Map.empty
  , currentLabel  = ""
  , variableVersions = Map.empty
  , stringDecls = []
  , stringMap = Map.empty
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

freshGlobal :: String -> CodeGen String
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

addLocal :: Name -> String -> CodeGen (String)
addLocal name addr = do
  state <- get
  let locals = localVars state
  case Map.lookup name locals of
    Just existingAddr -> do
      index <- newIndex
      let newAddr = addr ++ "_" ++ show index
      put state { localVars = Map.insert name newAddr locals }
      return newAddr
    Nothing -> do
      put state { localVars = Map.insert name addr locals }
      return addr

getLocal :: Name -> CodeGen String
getLocal name = do
  state <- get
  case Map.lookup name (localVars state) of 
    Just resolvedName -> return resolvedName
    Nothing -> return "getLocal error"

putLocals :: Map.Map Name String -> CodeGen ()
putLocals locals = do
  state <- get
  put state { localVars = locals }

getLocals :: CodeGen (Map.Map Name String)
getLocals = gets localVars

addLocalVarsType :: Name -> String -> CodeGen ()
addLocalVarsType name addr = do
  state <- get
  let locals = localVarsTypes state
  put state { localVarsTypes = Map.insert name addr locals }

getLocalVarsType :: Name -> CodeGen String
getLocalVarsType name = do
  state <- get
  case Map.lookup name (localVarsTypes state) of
    Just localVarType -> return localVarType
    Nothing -> error $ "Local var type not found for: " ++ name

putLocalsTypes :: Map.Map Name String -> CodeGen ()
putLocalsTypes locals = do
  state <- get
  put state { localVars = locals }

getLocalVarTypes :: CodeGen (Map.Map Name String)
getLocalVarTypes = gets localVars

getLocalsInfo :: CodeGen LocalsInfo
getLocalsInfo = do
  locals <- getLocals
  localsTypes <- getLocalVarTypes
  return (locals, localsTypes)

putLocalsInfo :: LocalsInfo -> CodeGen ()
putLocalsInfo (locals, localsTypes) = do
  putLocals locals
  putLocalsTypes localsTypes

addFunctionType :: Name -> String -> CodeGen ()
addFunctionType name ftype = do
  state <- get
  let functions = functionTypes state
  put state { functionTypes = Map.insert name ftype functions }

getFunctionType :: Name -> CodeGen String
getFunctionType name = do
  state <- get
  case Map.lookup name (functionTypes state) of
    Just functionType -> return functionType
    Nothing -> error $ "Function type not found for: " ++ name

addStringToMap :: String -> String -> CodeGen ()
addStringToMap name addr = do
  state <- get
  let strings = stringMap state
  put state { stringMap = Map.insert name addr strings }

getStringAddr :: Name -> CodeGen (Maybe String)
getStringAddr name = do
  state <- get
  return $ Map.lookup name (stringMap state)

getStringValue :: String -> CodeGen String
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

getStringDecls :: CodeGen [String]
getStringDecls = do
  state <- get
  let strings = "" : stringDecls state
  return (reverse strings)

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

getVariableVersionsByLabel :: String -> CodeGen (Map.Map String [String])
getVariableVersionsByLabel label = do
  state <- get
  let versions = variableVersions state
  case Map.lookup label versions of
    Just map -> return map
    Nothing -> return Map.empty

collectAllVariableVersions :: String -> String -> Set.Set String -> CodeGen (Map.Map String [String])
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

    return $ Map.union currentVars predVarsMerged

collectPredecessorVariables :: [String] -> Set.Set String -> CodeGen (Map.Map String [String])
collectPredecessorVariables preds visited = do
  predVars <- forM preds $ \predLabel -> do
    collectAllVariableVersions predLabel predLabel visited
  return $ Map.unions predVars

collectPredecessorVariablesForLabel :: String -> CodeGen (Map.Map String [String])
collectPredecessorVariablesForLabel label = do
  preds <- getPredecessors label
  collectPredecessorVariables preds Set.empty

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
      Map.insert var (version : Map.findWithDefault [] var varMap) varMap

runCodeGen :: CodeGenState -> CodeGen a -> (a, CodeGenState)
runCodeGen initialState codeGen = runState codeGen initialState

generatePredefinedFunctions :: [String]
generatePredefinedFunctions =
  [ "declare void @printInt(i32)",
    "declare void @printString(i8*)",
    "declare void @error()",
    "declare i32 @readInt()",
    "declare i8* @concat(i8*, i8*)",
    "declare i8* @readString()\n"
  ]

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
  let llvmReturnType = getType returnType
      functionName = getIdentName ident
  addFunctionType functionName llvmReturnType
    
generateFunction :: TopDef -> CodeGen [String]
generateFunction (FnDef _ returnType ident args block) = do
  let llvmReturnType = getType returnType
      functionName = getIdentName ident
  clearStateAtStartOfFun
  addFunctionType functionName llvmReturnType

  llvmArgs <- foldM generateFunctionArg [] args

  let header = "define " ++ llvmReturnType ++ " @" ++ functionName ++ "(" ++ intercalate ", " llvmArgs ++ ") {"

  entryLabel <- freshLabel
  addArgsToLabelVarMap entryLabel args
  emit $ entryLabel ++ ":"
  updateCurrentLabel entryLabel

  secondLabel <- freshLabel
  emit $ "  br label %" ++ secondLabel
  addEdge entryLabel secondLabel
  emit $ secondLabel ++ ":"
  updateCurrentLabel secondLabel

  generateBlockCode block
  blockCode <- getAccCode                                       -- odwrócony kod ciała funkcji bez SSA i Phi
  defaultForType <- getDefaultForType returnType
  let defaultReturnInstruction = if llvmReturnType == "void"
                          then "  ret void"
                          else "  ret " ++ llvmReturnType ++ " " ++ defaultForType

  let finalCodeWithReturn = if not (isRetInstruction defaultReturnInstruction (head blockCode))
                            then defaultReturnInstruction : blockCode 
                            else blockCode
  blockCodeWithSimplePhi <- processLabelsForPhi ("}\n" : finalCodeWithReturn)           -- wstawiam phi dla wszystkich zmiennych w blokach z kilkoma poprzednikami 
  blockWithSimplePhiAndSSA <- renameToSSA blockCodeWithSimplePhi                        -- wprowadzam SSA i uzupelniam informacje o zmiennych i wersjach w blokach
  updatedCode <- updateVariables blockWithSimplePhiAndSSA                               -- przemianowuję zmienne analizując przepływ bloków
  let processedAssCode = processAssignments updatedCode                                 -- pozbywam się sztucznych "%x = 2"
  return $ header : processedAssCode

isRetInstruction :: String -> String -> Bool
isRetInstruction returnInstruction line = "ret" `elem` words line

addArgsToLabelVarMap :: String -> [Arg] -> CodeGen ()
addArgsToLabelVarMap label args = do
  forM_ args $ \(Arg _ _ ident) -> do
    let argName = getIdentName ident
    updateVariableVersion label argName ('%':argName)

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
    predsVarVer <- collectAllVariableVersions label label Set.empty
    case predecessors of
      []     -> return []
      [_]    -> return []
      preds  -> do
        localVars <- gets localVarsTypes
        let phiInstrs = map (generatePhiInstr preds predsVarVer) (Map.toList localVars)
        return phiInstrs

generatePhiInstr :: [String] -> Map.Map String [String] -> (Name, String) -> String
generatePhiInstr predecessors predsVarVer (varName, varType) =
    case Map.lookup varName predsVarVer of
        Just (head:_) -> 
            "  " ++ head ++ " = phi " ++ varType ++ " " ++ generatePhiSources predecessors head
        _ -> 
            "  %" ++ varName ++ " = phi " ++ varType ++ " " ++ generatePhiSources predecessors ('%':varName)

generatePhiSources :: [String] -> String -> String
generatePhiSources preds varName =
    intercalate ", " [ "[" ++ varName ++ ", %" ++ pred ++ "]" | pred <- preds ]

getPredecessors :: String -> CodeGen [String]
getPredecessors label = do
  flowGraph <- gets flowGraph
  let predecessors = [predLabel | (predLabel, successors) <- Map.toList flowGraph, label `elem` successors]
  return predecessors

renameToSSA :: [String] -> CodeGen [String]
renameToSSA codeLines = process codeLines Map.empty []
  where
    process [] _ acc = return (reverse acc)
    process (line:rest) varVersions acc                                 -- varVersions służy do poprawnego określania kolejnej wersji dla zmiennej
      | Just (var, expr) <- parseAssignment line = do
          let newVersion = Map.findWithDefault (-1) var varVersions + 1
              newVarName = "%" ++ var ++ "." ++ show newVersion
              newLine = "  " ++ newVarName ++ " = " ++ renameVariables expr varVersions
              updatedVarVersions = Map.insert var newVersion varVersions
          currentLabel <- getCurrentLabel
          updateVariableVersion currentLabel var newVarName             -- dodaje informacje o zmiennych w bloku
          if '_' `elem` var then do
            let varWithoutRedeclaration = removeRedeclarationNumber var
            updateVariableVersion currentLabel varWithoutRedeclaration newVarName
            process rest updatedVarVersions (newLine : acc)
          else 
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
    isValidChar c = isAlphaNum c || c == '.' || c == '_'


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

updateVariables :: [String] -> CodeGen [String]
updateVariables code = do
  initialState <- collectPredecessorVariablesForLabel "L0" 

  (_, processedLines) <- foldM processLine (initialState, []) code
  return $ reverse processedLines

mapToString :: Map.Map String [String] -> String
mapToString m =
    let entries = Map.toList m
        formatEntry (key, values) = key ++ " -> [" ++ intercalate ", " values ++ "]"
    in intercalate "\n" (map formatEntry entries)

processLine :: (Map.Map String [String], [String]) -> String -> CodeGen (Map.Map String [String], [String])
processLine (versions, processedLines) line = do        -- versions a -> [%a.0]
  if not (null line) && last line == ':' then
    let label = init line in do
      updateCurrentLabel label
      versions <- collectPredecessorVariablesForLabel label
      return (versions, line : processedLines)
      -- lbl <- getVariableVersionsByLabel label
      -- return (versions, line : mapToString lbl  : processedLines)
  else if "br" `elem` words line then 
    return (versions, line : processedLines)
  else
    case parse phiParser "" line of
      Right (var, typ, args) -> do          -- %var, args=(%var, label??)
        updatedArgs <- mapM resolvePhiArg args
        let updatedVersions = Map.insertWith (++) (tail (removeVersion var)) [var] versions
        let updatedPhi = "  " ++ var ++ " = phi " ++ typ ++ " " ++ formatPhiArgs updatedArgs
        return (updatedVersions, updatedPhi : processedLines)
      Left _ -> do
        let wordsInLine = words line
        if "=" `elem` wordsInLine then
          let (lhs, rhs) = splitAssignment line
              varVer = trim lhs
              updatedVersions = Map.insertWith (++) (removeVersion (tail varVer)) [varVer] versions
              assignment = lhs ++ "="
          in do
            case parse (lineParser versions) "" rhs of
              Right updatedLine -> return (updatedVersions, (assignment ++ updatedLine) : processedLines)
              Left _ -> return (updatedVersions, (assignment ++ rhs) : processedLines)
        else
          case parse (lineParser versions) "" line of
            Right updatedLine -> return (versions, updatedLine : processedLines)
            Left _ -> return (versions, line :  processedLines) 

splitAssignment :: String -> (String, String)
splitAssignment line = 
  let (lhs, rest) = span (/= '=') line
      rhs = drop 1 rest
  in (lhs, rhs)

resolvePhiArg ::  (String, String) -> CodeGen (String, String)
resolvePhiArg (operand, label) = do
  varVersions <- collectAllVariableVersions label label Set.empty
  let updated = if head operand == '%' 
                  then case Map.lookup (removeVersionAndRedeclaration (tail operand)) varVersions of
                    Just versions -> 
                        if operand `elem` versions then operand
                        else head versions
                    Nothing -> operand  -- temp
                else operand    -- literal
                  
  return (updated, label)

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
                        if name `elem` versions then '%':name
                        else head versions
                    Nothing  -> "%"++name      -- zmienna temp
    return updated

removeVersionAndRedeclaration :: String -> String
removeVersionAndRedeclaration s = removeRedeclarationNumber $ removeVersion s

removeVersion :: String -> String
removeVersion s = reverse $ drop 1 $ dropWhile (/= '.') $ reverse s

removeRedeclarationNumber :: String -> String
removeRedeclarationNumber s = do
  if '_' `elem` s
     then drop 1 $ dropWhile (/= '_') $ reverse s
     else s

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
    string "["
    operand <- many1 (noneOf " ,]")
    spaces
    string ", %"
    label <- many1 (noneOf " ]")
    string "]"
    return (operand, label)

formatPhiArgs :: [(String, String)] -> String
formatPhiArgs args = intercalate ", " $ map (\(operand, label) -> "[" ++ operand ++ ", %" ++ label ++ "]") args

generateFunctionArg :: [String] -> Arg -> CodeGen [String]
generateFunctionArg argsCode (Arg _ argType ident) = do
  let llvmType = getType argType
      argName = getIdentName ident
  localName <- addLocal argName ("%" ++ argName)
  addLocalVarsType argName llvmType
  return (argsCode ++ [llvmType ++ " " ++ localName])

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
  preBlockLocalsInfo <- getLocalsInfo
  currLabel <- getCurrentLabel
  startBlockLabel <- freshLabel
  endBlockLabel <- freshLabel

  emit $ "  br label %" ++ startBlockLabel
  emit $ startBlockLabel ++ ":"

  addEdge currLabel startBlockLabel
  addEdge currLabel endBlockLabel
  updateCurrentLabel startBlockLabel

  mapM_ processStmt stmts

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
  unless doesTrueContainReturn $ addEdge trueLabel' endLabel'
  unless doesFalseContainReturn $ addEdge falseLabel' endLabel'

  genCond cond trueLabel' falseLabel'

  emit $ trueLabel' ++ ":"
  updateCurrentLabel trueLabel'
  mapM_ processStmt trueStmts
  unless doesTrueContainReturn $ emit $ "  br label %" ++ endLabel'
  putLocalsInfo preBlockLocalsInfo

  emit $ falseLabel' ++ ":"
  updateCurrentLabel falseLabel'
  mapM_ processStmt falseStmts
  unless doesFalseContainReturn $ emit $ "  br label %" ++ endLabel'
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
  updateCurrentLabel trueLabel'

  mapM_ processStmt stmts
  unless doesStmtContainReturn $ emit $ "  br label %" ++ endLabel'
  emit $ endLabel' ++ ":"
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
  updateCurrentLabel bodyLabel'
  mapM_ processStmt stmts
  putLocalsInfo preBlockLocalsInfo

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

extractStmts :: Stmt -> [Stmt]
extractStmts stmt = case stmt of
  BStmt _ (Block _ stmts) -> stmts
  statement -> [statement] 

generateVarDecl :: Type -> Item -> CodeGen ()
generateVarDecl varType (NoInit _ ident) = do
  defaultValue <- getDefaultForType varType
  let variableName = "%" ++ getIdentName ident
      llvmType = getType varType
  localName <- addLocal (getIdentName ident) variableName
  addLocalVarsType (getIdentName ident) llvmType
  currLbl <- getCurrentLabel
  updateVariableVersion currLbl (getIdentName ident) localName
  emit $ "  " ++ localName ++ " = " ++ defaultValue

generateVarDecl varType (Init _ ident expr) = do
  exprCode <- generateExprCode expr
  let variableName = "%" ++ getIdentName ident
      llvmType = getType varType
  localName <- addLocal (getIdentName ident) variableName
  addLocalVarsType (getIdentName ident) llvmType
  currLbl <- getCurrentLabel
  updateVariableVersion currLbl (getIdentName ident) localName
  emit $ "  " ++ localName ++ " = " ++ exprCode

assignString :: String -> CodeGen String      -- tworzy lub zwraca istniejacy global dla string
assignString value = do
  state <- get
  let escapedValue = escapeString value
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
  globalAddress <- assignString (escapeString value)
  tempVar <- freshTemp
  let strLength = length value + 1
      code = "  " ++ tempVar ++ " = bitcast [" ++ show strLength ++ " x i8]* " ++ globalAddress ++ " to i8*"
  emit code
  return tempVar
generateExprCode (EVar _ ident) = do                                                -- zwraca lokalne zmienne (dla string też)
  getLocal (getIdentName ident)
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
  addEdge trueLabel endLabel
  addEdge falseLabel endLabel
  genCond (EAnd Nothing expr1 expr2) trueLabel falseLabel

  emit $ trueLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 0"
  emit $ "  br label %" ++ endLabel

  emit $ falseLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 1"
  emit $ "  br label %" ++ endLabel

  emit $ endLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = phi i1 [" ++ tempVar ++ ", %" ++ trueLabel ++ "], [" ++ tempVar ++ ", %" ++ falseLabel ++ "]"  
  return tempVar
generateExprCode (EOr _ expr1 expr2) = do
  trueLabel <- freshLabel
  falseLabel <- freshLabel
  endLabel <- freshLabel
  tempVar <- freshTemp
  addEdge trueLabel endLabel
  addEdge falseLabel endLabel

  genCond (EOr Nothing expr1 expr2) trueLabel falseLabel

  emit $ trueLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 0"
  emit $ "  br label %" ++ endLabel

  emit $ falseLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = xor i1 1, 1"
  emit $ "  br label %" ++ endLabel

  emit $ endLabel ++ ":"
  emit $ "  " ++ tempVar ++ " = phi i1 [" ++ tempVar ++ ", %" ++ trueLabel ++ "], [" ++ tempVar ++ ", %" ++ falseLabel ++ "]" 
  return tempVar

genBinOp :: String -> Expr -> Expr -> CodeGen String    -- zmienna tymczasowa, do eliminacji w LCS
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

  -- addEdge currLabel midLabel
  -- addEdge currLabel lFalse

  genCond expr1 midLabel lFalse
  updateCurrentLabel midLabel

  emit $ midLabel ++ ":"
  genCond expr2 lTrue lFalse
genCond (EOr _ expr1 expr2) lTrue lFalse = do
  currLabel <- getCurrentLabel
  midLabel <- freshLabel

  -- addEdge currLabel lTrue
  -- addEdge currLabel midLabel

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

getDefaultForType :: Type -> CodeGen String
getDefaultForType (Int _) = return "0"
getDefaultForType (Str _) = do 
  globalAddress <- assignString ""
  return $ "bitcast [1 x i8]* " ++ globalAddress ++ " to i8*"
getDefaultForType (Bool _) = return "false"
getDefaultForType _ = return ""

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar c = case c of
      '\n' -> "\\0A"
      '\t' -> "\\09"
      '\r' -> "\\0D"
      '\\' -> "\\5C"
      '"'  -> "\\22" 
      '\0' -> "\\00" 
      c -> [c]

processAssignments :: [String] -> [String]
processAssignments finalCode = processLines finalCode Map.empty
  where
    processLines :: [String] -> Map.Map String String -> [String]
    processLines [] _ = []
    processLines (line : rest) assignments
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

    allEqual :: Eq a => [a] -> Bool
    allEqual [] = True
    allEqual (x:xs) = all (== x) xs

replaceVars :: String -> Map.Map String String -> String
replaceVars line assignments =
    case parse (lineParserAss assignments) "" line of
        Left err  -> error $ "Parse error: " ++ show err
        Right res -> res

lineParserAss :: Map.Map String String -> Parser String
lineParserAss assignments = do
    tokens <- many (variableParserAss assignments <|> nonVariable)
    return $ concat tokens

variableParserAss :: Map.Map String String -> Parser String
variableParserAss assignments = do
    char '%'
    let a = concatMap (\(k, v) -> k ++ ": " ++ v ++ "\n") (Map.toList assignments)
    varName <- many1 (alphaNum <|> char '_' <|> char '.')
    let updated = lookUpAssignment assignments varName
    return updated

lookUpAssignment :: Map.Map String String -> String -> String
lookUpAssignment assignments varName = do
  case Map.lookup ("%" ++ varName) assignments of
                      Just rhs ->  if head rhs == '%' 
                        then lookUpAssignment assignments (tail rhs)
                        else rhs
                      Nothing  -> "%" ++ varName


trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace
  
-- concatMap (\(k, v) -> k ++ ": " ++ v ++ "\n") (Map.toList myMap)