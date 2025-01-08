{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Optimizations where

import AbsLatte
import qualified AbsLatte
import Control.Monad (foldM, mapM)
import qualified Data.Map as Map
import Prelude (Bool (..), Either (..), Eq, Int, Integer, Maybe (..), Show, String, any, div, error, foldl, head, length, map, mod, not, null, otherwise, return, reverse, show, unlines, unwords, (!!), ($), (&&), (*), (+), (++), (-), (/=), (<), (<=), (==), (>), (>=), (||))

optimizeProgram :: Program -> Program
optimizeProgram (Program pos topDefs) =
  Program pos (map optimizeTopDef topDefs)
  where
    optimizeTopDef :: TopDef -> TopDef
    optimizeTopDef (FnDef pos retType name args block) =
      FnDef pos retType name args (optimizeBlock block)

    optimizeBlock :: Block -> Block
    optimizeBlock (Block pos stmts) =
      Block pos (removeDeadCodeAfterReturn stmts)

removeDeadCodeAfterReturn :: [Stmt] -> [Stmt]
removeDeadCodeAfterReturn stmts = go stmts False
  where
    go [] _ = []
    go (stmt : rest) hasReturned
      | hasReturned = []
      | otherwise = case stmt of
          Ret _ _ -> stmt : go rest True
          VRet _ -> stmt : go rest True
          BStmt pos blk -> BStmt pos (removeDeadCodeFromBlock blk) : go rest hasReturned
          Cond pos cond trueStmt -> Cond pos cond (head (go [trueStmt] False)) : go rest hasReturned
          CondElse pos cond trueStmt falseStmt ->
            CondElse
              pos
              cond
              (head (go [trueStmt] False))
              (head (go [falseStmt] False))
              : go rest hasReturned
          While pos cond body -> While pos cond (head (go [body] False)) : go rest hasReturned
          _ -> stmt : go rest hasReturned

    removeDeadCodeFromBlock :: Block -> Block
    removeDeadCodeFromBlock (Block pos stmts) =
      Block pos (go stmts False)
