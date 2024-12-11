module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>), Bool(True)
  , String, (++), concat, unlines
  , Show, show
  , IO, (>>), (>>=), mapM_, putStrLn
  , FilePath
  , getContents, readFile
  , return
  )
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )
import System.IO ( writeFile, hPutStrLn, stderr )
import System.Process     ( callCommand )
import System.Directory   ( createDirectoryIfMissing )
import System.FilePath    ( takeBaseName, takeDirectory )

import AbsLatte   ( Program(..) )
import LexLatte   ( Token, mkPosToken )
import ParLatte   ( pProgram, myLexer )
import Frontend  ( checkSemantics )
import Backend (generateLLVM)
import Optimizations (optimizeProgram)
import PrintLatte (Print, printTree)


type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ hPutStrLn stderr s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p f

run :: Verbosity -> ParseFun Program -> FilePath -> String -> IO ()
run v p f s =
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
          let llvmCode = generateLLVM optimizedTree
          let baseName = takeBaseName f
          let outputDir = takeDirectory f
          createDirectoryIfMissing True outputDir

          let llFilePath = outputDir ++ "/" ++ baseName ++ ".ll"
          let bcFilePath = outputDir ++ "/" ++ baseName ++ ".bc"

          writeFile llFilePath llvmCode
          -- callCommand $ "llvm-as " ++ llFilePath ++ " -o " ++ bcFilePath

          showTree v optimizedTree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

-- TODO usunac PrintLatte i ta funkcje
showTree :: Int -> Program -> IO ()
showTree v tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  (files)         Parse and compile content of files verbosely."
    , "  -s (files)      Silent mode. Parse and compile content of files silently."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> usage
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

