module Optimizations where

import Control.Monad.State
import Data.Char (isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Map as Map
import Text.Parsec (alphaNum, anyToken, between, char, many, many1, noneOf, parse, sepBy, skipMany, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)
import Data.Binary.Builder (flush)
import GHC.RTS.Flags (DebugFlags(block_alloc))
import GHC.Enum (succError)

type LabelOpt = String

type FlowGraphOpt = Map.Map LabelOpt [LabelOpt]

type BlockName = String

type BlocksWithCode = Map.Map BlockName [String]

data SubElimState = SubElimState
  { assignments :: Map.Map String String, -- "rhs czyli co się powtórzy" -> adres z pierwszym wystąpieniem
    replacements :: Map.Map String String, -- var1 -> var2 -- var1 zostanie zastąpione przez var2
    codeInBlocks :: BlocksWithCode,
    varOccurrences :: Map.Map String Int,
    flowGraphOpt :: FlowGraphOpt,
    currentBlock :: String,
    blocksInOrder :: [String]
  }
  deriving (Show)

initialStateOpt :: SubElimState
initialStateOpt =
  SubElimState
    { assignments = Map.empty,
      replacements = Map.empty,
      codeInBlocks = Map.empty,
      varOccurrences = Map.empty,
      flowGraphOpt = Map.empty,
      currentBlock = "",
      blocksInOrder = []
    }

type SubElim = State SubElimState

addAssign :: String -> String -> SubElim ()
addAssign rhs lhs = do
  state <- get
  let assigns = assignments state
  put state {assignments = Map.insert rhs lhs assigns}
  setVarOccurrence lhs 0

getAssignVar :: String -> SubElim (Maybe String)
getAssignVar rhs = do
  state <- get
  return $ Map.lookup rhs (assignments state)

getAssignments :: SubElim (Map.Map String String)
getAssignments = do gets assignments

addReplacement :: String -> String -> SubElim ()
addReplacement var1 var2 = do
  state <- get
  let replcs = replacements state
  put state {replacements = Map.insert var1 var2 replcs}

getReplacement :: String -> SubElim (Maybe String)
getReplacement var = do
  state <- get
  return $ Map.lookup var (replacements state)

getReplacements :: SubElim (Map.Map String String)
getReplacements = do gets replacements

setVarOccurrence :: String -> Int -> SubElim ()
setVarOccurrence var count = do
  state <- get
  let occurrences = varOccurrences state
      updatedOccurrences = Map.insert var count occurrences
  put state {varOccurrences = updatedOccurrences}

getVarOccurrence :: String -> SubElim Int
getVarOccurrence var = do
  state <- get
  let occurrences = varOccurrences state
  return $ Map.findWithDefault 0 var occurrences

increaseVarOccurrence :: String -> SubElim ()
increaseVarOccurrence var = do
  counter <- getVarOccurrence var
  setVarOccurrence var (counter + 1)

putFlowGraph :: FlowGraphOpt -> SubElim ()
putFlowGraph fG = do
  state <- get
  put state {flowGraphOpt = fG}

addLineToBlock :: String -> SubElim ()
addLineToBlock newLine = do
  state <- get
  let blockName = currentBlock state
      blocks = codeInBlocks state
      updatedBlocks = Map.insertWith (++) blockName [newLine] blocks
  put state {codeInBlocks = updatedBlocks}

clearCodeInBlock :: BlockName -> SubElim ()
clearCodeInBlock block = do
  state <- get
  let blocks = codeInBlocks state
      updatedBlocks = Map.insert block [] blocks
  put state { codeInBlocks = updatedBlocks }
  return ()

getCodeInBlock :: BlockName -> SubElim [String]
getCodeInBlock blockName = do
  state <- get
  let blocks = codeInBlocks state
      code = Map.findWithDefault [] blockName blocks
  return (reverse code)

putCodeInBlock :: BlockName -> [String] -> SubElim ()
putCodeInBlock blockName newCode = do
  state <- get
  let blocks = codeInBlocks state
      updatedBlocks = Map.insert blockName newCode blocks
  put state { codeInBlocks = updatedBlocks }

getCodeInAllBlocks :: SubElim [String]
getCodeInAllBlocks = do
  state <- get
  let blocks = codeInBlocks state
  order <- getBlocksInOrder
  foldM addCode [] order
  where
    addCode acc blockName = do
      code <- getCodeInBlock blockName
      return (acc ++ code)

getCodeInAllBlocksAndFlush :: SubElim [String]
getCodeInAllBlocksAndFlush = do
  state <- get
  code <- getCodeInAllBlocks
  put state {codeInBlocks = Map.empty}
  return code

updateCurrentBlock :: String -> SubElim ()
updateCurrentBlock newBlock = do
  state <- get
  put state {currentBlock = newBlock}

getCurrentBlock :: SubElim String
getCurrentBlock = do
  state <- get
  return (currentBlock state)

getZeroOccurrencesAndFlush :: SubElim [String]
getZeroOccurrencesAndFlush = do
  state <- get
  let varOccurr = varOccurrences state
  put state { varOccurrences = Map.empty } 
  return [var | (var, count) <- Map.toList varOccurr, count == 0]

addBlockToOrder :: LabelOpt -> SubElim ()
addBlockToOrder label = do
  state <- get
  let ordered = blocksInOrder state
  put state { blocksInOrder = label : ordered}

getBlocksInOrder :: SubElim [String]
getBlocksInOrder = do
  state <- get
  let ordered = blocksInOrder state
  return $ reverse ordered

clearAssignments :: SubElim ()
clearAssignments = do
  state <- get
  put state {assignments = Map.empty}

clearOccurences :: SubElim ()
clearOccurences = do
  state <- get
  put state {varOccurrences = Map.empty}

getSuccessors :: BlockName -> SubElim [BlockName]
getSuccessors blockName = do
  fg <- gets flowGraphOpt
  return $ Map.findWithDefault [] blockName fg 

runOptimizations :: SubElimState -> SubElim a -> (a, SubElimState)
runOptimizations initialStateOpt codeGen = runState codeGen initialStateOpt

optimize :: FlowGraphOpt -> [String] -> BlocksWithCode -> SubElim [String]        ---rework
optimize fg order codeBlocks = do
  state <- get
  put state {flowGraphOpt = fg, blocksInOrder = order, codeInBlocks = codeBlocks}
  orderedBlocks <- getBlocksInOrder
  performLCSE orderedBlocks
  --GCSE usuwać nieużywane zmienne jak phi albo tempy
  performGCSE (head orderedBlocks)
  -- return [mapToString2 fg] 
  
mapToString2 :: Map.Map String [String] -> String
mapToString2 m =
  let entries = Map.toList m
      formatEntry (key, values) = key ++ " -> [" ++ intercalate ", " values ++ "]"
   in intercalate "\n" (map formatEntry entries)

performLCSE :: [String] -> SubElim [String]
performLCSE orderedBlocks = do
  initialCode <- getCodeInAllBlocks

  let optimizeStep code = do
        clearOccurences
        mapM_ optimizeBlockLocal orderedBlocks
        updatedCode <- getCodeInAllBlocks 
        if updatedCode == code
          then return updatedCode
          else optimizeStep updatedCode 

  optimizeStep initialCode

optimizeBlockLocal :: String -> SubElim ()
optimizeBlockLocal blockName = do
  code <- getCodeInBlock blockName
  clearAssignments
  updatedCodeReversed <- foldM adjustCodeLocal [] code
  putCodeInBlock blockName updatedCodeReversed
  return ()

adjustCodeLocal :: [String] -> String -> SubElim [String]
adjustCodeLocal accCode line
  | "=" `isInfixOf` line = do
      let (lhs, rhs) = splitAssignment line
      if ("phi" `isInfixOf` rhs) || ("@readInt()" `isInfixOf` rhs || "@readString()" `isInfixOf` rhs)         -- readInt i readString zwrócą mogą zwrócić inną wartość mimo takich samych wywołań. Nie wolno optymalizować. Żywotność phi będzie określana w GCSE
        then do
          updatedLine <- replaceVars line
          return $ updatedLine : accCode
        else do
          updatedLine <- replaceVars line
          let (lhs, rhs) = splitAssignment updatedLine
          maybeRepl <- getAssignVar rhs
          case maybeRepl of
            Just replacement -> do
              addReplacement lhs replacement
              return accCode
            Nothing -> do
              addAssign rhs lhs
              return $ updatedLine : accCode
  | otherwise = do
      updatedLine <- replaceVars line
      return $ updatedLine : accCode

extractAssignment :: String -> (String, String)
extractAssignment line =
  let parts = words line
      lhs = head parts
      rhs = last parts
   in (lhs, rhs)

replaceVars :: String -> SubElim String
replaceVars line = do
  replacements <- getReplacements
  case parse (lineParserWithVars replacements) "" line of
    Left err -> error $ "replaceVars: " ++ show err
    Right (res, vars) -> do
      mapM_ increaseVarOccurrence vars
      return res

lineParserWithVars :: Map.Map String String -> Parser (String, [String])
lineParserWithVars replacements = do
  tokensAndVars <- many (variableParserWithVars replacements <|> nonVariableWithVars)
  let (tokens, vars) = unzip tokensAndVars
  return (concat tokens, concat vars)

variableParserWithVars :: Map.Map String String -> Parser (String, [String])
variableParserWithVars replacements = do
  char '%'
  varAddr <- many1 (alphaNum <|> char '_' <|> char '.')
  let updated = lookUpReplacements replacements ('%' : varAddr)
  return (updated, ['%' : varAddr])

lookUpReplacements :: Map.Map String String -> String -> String
lookUpReplacements replacements varAddr = do
  case Map.lookup varAddr replacements of
    Just updatedVar ->
      if head updatedVar == '%'
        then lookUpReplacements replacements updatedVar
        else updatedVar
    Nothing -> varAddr

nonVariableWithVars :: Parser (String, [String])
nonVariableWithVars = do
  token <- many1 (noneOf "%")
  return (token, [])

splitAssignment :: String -> (String, String)
splitAssignment line =
  let (lhs, rest) = span (/= '=') line
      rhs = drop 1 rest
   in (trim lhs, trim rhs)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
  
performGCSE :: String -> SubElim [String]
performGCSE blockName = do
  initialCode <- getCodeInAllBlocks

  let optimizeStep code = do
        optimizeBlocksGlobal [] blockName
        updatedCode <- getCodeInAllBlocks 
        if updatedCode == code
          then return updatedCode
          else do
            clearAssignments
            clearOccurences
            optimizeStep updatedCode 

  optimizeStep initialCode

optimizeBlocksGlobal :: [BlockName] -> BlockName -> SubElim ()
optimizeBlocksGlobal visited blockName = do
  if blockName `elem` visited 
    then return ()
    else do
      let updatedVisited = blockName : visited
      codeInBlock <- getCodeInBlock blockName
      adjustedCodeReversed <- foldM adjustCodeGlobal [] codeInBlock
      putCodeInBlock blockName adjustedCodeReversed

      succ <- getSuccessors blockName
      if null succ
        then return ()
        else mapM_ (optimizeBlocksGlobal updatedVisited) succ

adjustCodeGlobal :: [String] -> String -> SubElim [String]
adjustCodeGlobal accCode line
  | "phi" `isInfixOf` line = do
      updatedLine <- replaceVars line
      let (lhs, rhs) = splitAssignment updatedLine
      setVarOccurrence lhs 0
      return $ updatedLine : accCode
  
  | "@readInt()" `isInfixOf` line || "@readString()" `isInfixOf` line = do
      updatedLine <- replaceVars line
      return $ updatedLine : accCode

  | "=" `isInfixOf` line = do
      updatedLine <- replaceVars line
      let (lhs, rhs) = splitAssignment updatedLine
      maybeRepl <- getAssignVar rhs
      case maybeRepl of
        Just replacement -> do
          addReplacement lhs replacement
          return accCode
        Nothing -> do
          addAssign rhs lhs
          return $ updatedLine : accCode

  | otherwise = do
      updatedLine <- replaceVars line
      return $ updatedLine : accCode
