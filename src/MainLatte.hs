module Main where

import AbsLatte (Program (..))
import Backend (CodeGenState, generateLLVM, initialState, runCodeGen)
import Control.Monad (when)
import Frontend (checkSemantics)
import LexLatte (Token, mkPosToken)
import Midend (optimizeProgram)
import ParLatte (myLexer, pProgram)
import PrintLatte (Print, printTree)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName, takeDirectory)
import System.IO (hPutStrLn, stderr, writeFile)
import System.Process (callCommand, readProcess)
import Prelude
import Data.List

type Err = Either String

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ hPutStrLn stderr s

runFile :: Int -> Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile optLevel v p f = readFile f >>= run optLevel v p f

run :: Int -> Verbosity -> ParseFun Program -> FilePath -> String -> IO ()
run optLevel  v p f s =
  case p ts of
    Left err -> do
      hPutStrLn stderr "ERROR\nParse Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      hPutStrLn stderr err
      exitFailure
    Right tree -> do
      case checkSemantics tree of
        Left err -> do
          hPutStrLn stderr "ERROR\nSemantic Error: \n"
          hPutStrLn stderr err
          exitFailure
        Right _ -> do
          hPutStrLn stderr "OK\n"
          let optimizedTree = optimizeProgram tree
          let llvmCode = fst $ runCodeGen initialState $ generateLLVM optimizedTree optLevel
          let baseName = takeBaseName f
          let outputDir = takeDirectory f
          createDirectoryIfMissing True outputDir

          let llFilePath = outputDir ++ "/" ++ baseName ++ ".ll"
          let bcFilePath = outputDir ++ "/" ++ baseName ++ ".bc"
          let libPath = "./lib/runtime.bc"

          writeFile llFilePath llvmCode
          callCommand $ "llvm-as " ++ llFilePath ++ " -o " ++ bcFilePath
          callCommand $ "llvm-link " ++ bcFilePath ++ " " ++ libPath ++ " -o " ++ bcFilePath
          callCommand $ "lli " ++ bcFilePath
  where
    ts = myLexer s
    showPosToken ((l, c), t) = concat [show l, ":", show c, "\t", show t]

showTree :: Int -> Program -> IO ()
showTree v tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  (files)         Parse and compile content of files verbosely.",
        "  -s (files)      Silent mode. Parse and compile content of files silently."
      ]

parseOptimizationLevel :: [String] -> Int
parseOptimizationLevel options =
  case filter ("-o" `isPrefixOf`) options of
    []         -> 2 -- domyślny poziom optymalizacji
    ["-o0"]    -> 0
    ["-o1"]    -> 1
    ["-o2"]    -> 2
    _          -> error "Invalid optimization level. Use -o0, -o1, or -o2."
   
main :: IO ()
main = do
  args <- getArgs
  let (options, files) = partition ("-" `isPrefixOf`) args
  let optLevel = parseOptimizationLevel options
  if null files
    then usage
    else mapM_ (runFile optLevel 2 pProgram) files
