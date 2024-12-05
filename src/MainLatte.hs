module Main where

import Prelude
  ( ($), (.)
  , Either(..)
  , Int, (>)
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

import AbsLatte   ( Program(..) )
import LexLatte   ( Token, mkPosToken )
import ParLatte   ( pProgram, myLexer )
import Frontend  ( checkSemantics )
import System.IO (hPutStrLn, stderr)
import Backend (generateLLVM)
import PrintLatte (Print, printTree)

type Err        = Either String
type ParseFun a = [Token] -> Err a
type Verbosity  = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ hPutStrLn stderr s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
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
          showTree v (generateLLVM tree)
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

-- TODO usunac PrintLatte i ta funkcje
showTree :: Int -> Program -> IO ()
showTree v tree = do
  -- putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree


main :: IO ()
main = do
  args <- getArgs
  case args of
    []         -> getContents >>= run 2 pProgram
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs

