module Optimizations where

import Control.Monad.State
import Data.Char (isSpace)
import Data.List (find, intercalate, isInfixOf, isPrefixOf, nub)
import qualified Data.Map as Map
import Text.Parsec (alphaNum, anyToken, between, char, many, many1, noneOf, parse, sepBy, skipMany, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

type LabelOpt = String

type FlowGraphOpt = Map.Map LabelOpt [LabelOpt]

type BlockName = String

data SubElimState = SubElimState
  { assignments :: Map.Map String String, -- "rhs czyli co się powtórzy" -> adres z pierwszym wystąpieniem
    replacements :: Map.Map String String, -- var1 -> var2 -- var1 zostanie zastąpione przez var2
    codeInBlocks :: Map.Map BlockName [String],
    varOccurrences :: Map.Map String Int,
    flowGraphOpt :: FlowGraphOpt,
    currentBlock :: String
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
      currentBlock = ""
    }

type SubElim = State SubElimState

addAssign :: String -> String -> SubElim ()
addAssign lhs rhs = do
  state <- get
  let assigns = assignments state
  put state {assignments = Map.insert rhs lhs assigns}
  setVarOccurrence lhs 0

getAssignVar :: String -> SubElim (Maybe String)
getAssignVar rhs = do
  state <- get
  return $ Map.lookup rhs (assignments state)

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
getReplacements = do
  state <- get
  let replcs = replacements state
  return replcs

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

getCodeInBlock :: BlockName -> SubElim [String]
getCodeInBlock blockName = do
  state <- get
  let blocks = codeInBlocks state
      code = Map.findWithDefault [] blockName blocks
  return (reverse code)

getCodeInAllBlocks :: SubElim [String]
getCodeInAllBlocks = do
  state <- get
  let blocks = codeInBlocks state
  foldM addCode [] (Map.keys blocks)
  where
    addCode acc blockName = do
      code <- getCodeInBlock blockName
      return (acc ++ code)

updateCurrentBlock :: String -> SubElim ()
updateCurrentBlock newBlock = do
  state <- get
  put state {currentBlock = newBlock}

getCurrentBlock :: SubElim String
getCurrentBlock = do
  state <- get
  return (currentBlock state)

runOptimizations :: SubElimState -> SubElim a -> (a, SubElimState)
runOptimizations initialStateOpt codeGen = runState codeGen initialStateOpt

optimize :: [String] -> FlowGraphOpt -> SubElim [String]
optimize funCode flowGraph = do
  putFlowGraph flowGraph
  performLCSE funCode
--   return funCode
  getCodeInAllBlocks

performLCSE :: [String] -> SubElim [String]
performLCSE [] = return []
performLCSE (line : rest)
  | not (null line) && last line == ':' = do
      let label = init line
      updateCurrentBlock label
      addLineToBlock line
      restResult <- performLCSE rest
      return (line : restResult)
  | "=" `isInfixOf` line = do
      let (lhs, rhs) = splitAssignment line
      if "phi" `isInfixOf` rhs
        then do
          setVarOccurrence lhs 0                                                       --- todo liczyc wystapienia phi zeby wiedziec czy zbedne. wykonac lcse tyle razy az bez zmian
          addLineToBlock line
          restResult <- performLCSE rest
          return (line : restResult)
        else do
          let (lhs, rhs) = splitAssignment line
          maybeRepl <- getAssignVar rhs
          case maybeRepl of
            Just replacement -> do
              addReplacement lhs replacement
              performLCSE rest
            Nothing -> do
              addAssign lhs rhs
              updatedLine <- replaceVars line
              addLineToBlock updatedLine
              restResult <- performLCSE rest
              return (updatedLine : restResult)
  | otherwise = do
      updatedLine <- replaceVars line
      addLineToBlock updatedLine
      restResult <- performLCSE rest
      return (updatedLine : restResult)

extractAssignment :: String -> (String, String)
extractAssignment line =
  let parts = words line
      lhs = head parts
      rhs = last parts
   in (lhs, rhs)

replaceVars :: String -> SubElim String
replaceVars line = do
  replacements <- getReplacements
  case parse (lineParser replacements) "" line of
    Left err -> error $ "replaceVars: " ++ show err
    Right res -> return res

lineParser :: Map.Map String String -> Parser String
lineParser replacements = do
  tokens <- many (variableParser replacements <|> nonVariable)
  return $ concat tokens

variableParser :: Map.Map String String -> Parser String
variableParser replacements = do
  char '%'
  varAddr <- many1 (alphaNum <|> char '_' <|> char '.')
  let updated = lookUpReplacements replacements ('%' : varAddr)
  return updated

lookUpReplacements :: Map.Map String String -> String -> String
lookUpReplacements replacements varAddr = do
  case Map.lookup varAddr replacements of
    Just updatedVar ->
      if head updatedVar == '%'
        then lookUpReplacements replacements updatedVar
        else updatedVar
    Nothing -> varAddr

nonVariable :: Parser String
nonVariable = many1 (noneOf "%")

splitAssignment :: String -> (String, String)
splitAssignment line =
  let (lhs, rest) = span (/= '=') line
      rhs = drop 1 rest
   in (trim lhs, trim rhs)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace