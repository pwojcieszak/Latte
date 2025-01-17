module Optimizations where

import Control.Monad.State
import Data.Binary.Builder (flush)
import Data.Char (isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import GHC.Enum (succError)
import GHC.RTS.Flags (DebugFlags (block_alloc))
import StringParsers (lineParserWithVars, phiParser)
import Text.Parsec (parse)

type LabelOpt = String

type FlowGraphOpt = Map.Map LabelOpt [LabelOpt]

type BlockName = String

type BlocksWithCode = Map.Map BlockName [String]

type AssignmentMap = Map.Map String String

type ReplacementMap = Map.Map String String

data SubElimState = SubElimState
  { assignments :: AssignmentMap, -- "rhs czyli co się powtórzy" -> adres z pierwszym wystąpieniem
    replacements :: ReplacementMap, -- var1 -> var2 -- var1 zostanie zastąpione przez var2
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
  spyVarOccurrences lhs

getAssignVar :: String -> SubElim (Maybe String)
getAssignVar rhs = do
  state <- get
  return $ Map.lookup rhs (assignments state)

getAssignments :: SubElim AssignmentMap
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

getReplacements :: SubElim ReplacementMap
getReplacements = do gets replacements

spyVarOccurrences :: String -> SubElim ()
spyVarOccurrences var = do
  state <- get
  let occurrences = varOccurrences state
  case Map.lookup var occurrences of
    Just _ -> return ()
    Nothing -> do
      let updatedOccurrences = Map.insert var 0 occurrences
      put state {varOccurrences = updatedOccurrences}

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

decreaseVarOccurrence :: String -> SubElim ()
decreaseVarOccurrence var = do
  counter <- getVarOccurrence var
  setVarOccurrence var (counter - 1)

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
  put state {codeInBlocks = updatedBlocks}
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
  put state {codeInBlocks = updatedBlocks}

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
  res <- getZeroOccurrences
  clearOccurences
  return res

getZeroOccurrences :: SubElim [String]
getZeroOccurrences = do
  state <- get
  let varOccurr = varOccurrences state
  return [var | (var, count) <- Map.toList varOccurr, count == 0]

addBlockToOrder :: LabelOpt -> SubElim ()
addBlockToOrder label = do
  state <- get
  let ordered = blocksInOrder state
  put state {blocksInOrder = label : ordered}

getBlocksInOrder :: SubElim [String]
getBlocksInOrder = do
  state <- get
  let ordered = blocksInOrder state
  return $ reverse ordered

clearAssignments :: SubElim ()
clearAssignments = do
  state <- get
  put state {assignments = Map.empty}

clearReplacements :: SubElim ()
clearReplacements = do
  state <- get
  put state {replacements = Map.empty}

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

optimize :: Int -> FlowGraphOpt -> [String] -> BlocksWithCode -> SubElim [String]
optimize optLevel fg order codeBlocks = do
  state <- get
  put state {flowGraphOpt = fg, blocksInOrder = order, codeInBlocks = codeBlocks}
  orderedBlocks <- getBlocksInOrder
  case optLevel of
    0 -> getCodeInAllBlocks
    1 -> performLCSE orderedBlocks
    2 -> do
      performLCSE orderedBlocks
      performGCSE (head orderedBlocks)
  
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
  | "call" `isInfixOf` line = do
      replacements <- getReplacements
      updatedLine <- replaceVars line replacements
      return $ updatedLine : accCode

  | "phi" `isInfixOf` line = do
      replacements <- getReplacements
      updatedLine <- replaceVars line replacements
      case parse phiParser "" updatedLine of
        Right (var, typ, args) -> do
          let argValues = map fst args
          if allEqual (filter (/= var) argValues)
            then do
              addReplacement var (head (filter (/= var) argValues))
              return accCode
            else do
              return (updatedLine : accCode)
        Left _ -> do
          return (updatedLine : accCode)

      return $ updatedLine : accCode
  | "=" `isInfixOf` line = do
      let (lhs, rhs) = splitAssignment line
      replacements <- getReplacements
      updatedLine <- replaceVars line replacements
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
      replacements <- getReplacements
      updatedLine <- replaceVars line replacements
      return $ updatedLine : accCode

extractAssignment :: String -> (String, String)
extractAssignment line =
  let parts = words line
      lhs = head parts
      rhs = last parts
   in (lhs, rhs)

replaceVars :: String -> Map.Map String String -> SubElim String
replaceVars line replacements = do
  case parse (lineParserWithVars replacements) "" line of
    Left err -> error $ "replaceVars: " ++ show err
    Right (res, vars) -> do
      mapM_ increaseVarOccurrence vars
      return res

splitAssignment :: String -> (String, String)
splitAssignment line =
  let (lhs, rest) = span (/= '=') line
      rhs = drop 1 rest
   in (trim lhs, trim rhs)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs

performGCSE :: String -> SubElim [String]
performGCSE blockName = do
  initialCode <- getCodeInAllBlocks
  clearOccurences

  let optimizeStep code = do
        optimizeBlocksGlobal [] Map.empty Map.empty blockName
        deadVars <- getZeroOccurrencesAndFlush
        unless (null deadVars) $ do
          order <- getBlocksInOrder
          mapM_ (removeDeadCode deadVars) order

        updatedCode <- getCodeInAllBlocks

        if updatedCode == code
          then return updatedCode
          else optimizeStep updatedCode

  optimizeStep initialCode

optimizeBlocksGlobal :: [BlockName] -> AssignmentMap -> ReplacementMap -> BlockName -> SubElim ()
optimizeBlocksGlobal visited assignments replacements blockName = do
  if blockName `elem` visited
    then return ()
    else do
      let updatedVisited = blockName : visited
      codeInBlock <- getCodeInBlock blockName
      (adjustedCodeReversed, updatedAssignments, updatedReplacements) <- foldM adjustCodeGlobal ([], assignments, replacements) codeInBlock
      putCodeInBlock blockName adjustedCodeReversed

      succ <- getSuccessors blockName
      if null succ
        then return ()
        else do
          let prefix = take 2 blockName
              specialEndBlock = find (\s -> prefix `isPrefixOf` s && "end" `isInfixOf` s) succ
          case specialEndBlock of -- obecność END znaczyłaby że jesteśmy w COND, CONDELSE, WHILE, albo bool expression
            Just endBlock | "true" `isInfixOf` blockName || "false" `isInfixOf` blockName -> do
              let otherBlocks = filter (/= endBlock) succ
              mapM_ (optimizeBlocksGlobal updatedVisited updatedAssignments updatedReplacements) otherBlocks
              optimizeBlocksGlobal updatedVisited assignments updatedReplacements endBlock
            _ -> do
              if "mid" `isInfixOf` blockName
                then do
                  -- sprawdzam czy jestem w środku kodu skaczącego
                  let nextMidBlock = find (\s -> "mid" `isInfixOf` s) succ
                  case nextMidBlock of
                    Just midBlock -> do
                      let otherBlocks = filter (/= midBlock) succ
                      mapM_ (optimizeBlocksGlobal updatedVisited assignments updatedReplacements) otherBlocks
                      optimizeBlocksGlobal updatedVisited updatedAssignments updatedReplacements midBlock
                    _ -> mapM_ (optimizeBlocksGlobal updatedVisited assignments updatedReplacements) succ
                else mapM_ (optimizeBlocksGlobal updatedVisited updatedAssignments updatedReplacements) succ

adjustCodeGlobal :: ([String], AssignmentMap, ReplacementMap) -> String -> SubElim ([String], AssignmentMap, ReplacementMap)
adjustCodeGlobal (accCode, assignments, replacements) line
  | "phi" `isInfixOf` line = do
      updatedLine <- replaceVars line replacements 
      case parse phiParser "" updatedLine of
        Right (var, typ, args) -> do
          let argValues = map fst args
          if allEqual (filter (/= var) argValues)
            then do
              let updatedReplacements = Map.insert var (head (filter (/= var) argValues)) replacements
              return (accCode, assignments, updatedReplacements)
            else do
              let (lhs, rhs) = splitAssignment updatedLine
              decreaseVarOccurrence lhs
              return (updatedLine : accCode, assignments, replacements)
        Left _ -> do
          let (lhs, rhs) = splitAssignment updatedLine
          decreaseVarOccurrence lhs
          return (updatedLine : accCode, assignments, replacements)

  | "call" `isInfixOf` line = do
      updatedLine <- replaceVars line replacements
      return (updatedLine : accCode, assignments, replacements)
  | "=" `isInfixOf` line = do
      updatedLine <- replaceVars line replacements
      let (lhs, rhs) = splitAssignment updatedLine
      decreaseVarOccurrence lhs
      case Map.lookup rhs assignments of
        Just replacement -> do
          let updatedReplacements = Map.insert lhs replacement replacements
          return (accCode, assignments, updatedReplacements)
        Nothing -> do
          let updatedAssigns = Map.insert rhs lhs assignments
          return (updatedLine : accCode, updatedAssigns, replacements)
  | otherwise = do
      updatedLine <- replaceVars line replacements
      return (updatedLine : accCode, assignments, replacements)

removeDeadCode :: [String] -> String -> SubElim ()
removeDeadCode deadVars blockName = do
  code <- getCodeInBlock blockName
  updatedCodeReversed <- foldM (removeDeadCodeBlock deadVars) [] code
  putCodeInBlock blockName updatedCodeReversed
  return ()

removeDeadCodeBlock :: [String] -> [String] -> String -> SubElim [String]
removeDeadCodeBlock deadVars accCode line
  | "=" `isInfixOf` line = do
      let (lhs, rhs) = splitAssignment line
      if lhs `elem` deadVars
        then return accCode
        else return (line : accCode)
  | otherwise = return (line : accCode)