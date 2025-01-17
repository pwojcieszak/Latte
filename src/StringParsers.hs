module StringParsers where

import Data.Map qualified as Map
import Text.Parsec (alphaNum, anyToken, between, char, many, many1, noneOf, parse, sepBy, skipMany, space, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

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
        Nothing -> "%" ++ name
  return updated

nonVariable :: Parser String
nonVariable = many1 (noneOf "%")

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

lineParserCFG :: Map.Map String [String] -> Parser String
lineParserCFG varVersions = do
  tokens <- many (variableParserCFG varVersions <|> nonVariable)
  return $ concat tokens

variableParserCFG :: Map.Map String [String] -> Parser String
variableParserCFG varVersions = do
  char '%'
  name <- many1 (alphaNum <|> char '_' <|> char '.')
  let updated = case Map.lookup (removeVersionPar name) varVersions of
        Just versions -> head versions
        Nothing -> "%" ++ name -- zmienna label
  return updated

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

nonVariableWithVars :: Parser (String, [String])
nonVariableWithVars = do
  token <- many1 (noneOf "%")
  return (token, [])

lookUpAssignment :: Map.Map String String -> String -> String
lookUpAssignment assignments varAddr = do
  case Map.lookup varAddr assignments of
    Just rhs ->
      if head rhs == '%'
        then lookUpAssignment assignments rhs
        else rhs
    Nothing -> varAddr
removeVersionPar :: String -> String
removeVersionPar s = do
  if '.' `elem` s
    then reverse $ drop 1 $ dropWhile (/= '.') $ reverse s
    else s

lookUpReplacements :: Map.Map String String -> String -> String
lookUpReplacements replacements varAddr = do
  case Map.lookup varAddr replacements of
    Just updatedVar ->
      if head updatedVar == '%'
        then lookUpReplacements replacements updatedVar
        else updatedVar
    Nothing -> varAddr
