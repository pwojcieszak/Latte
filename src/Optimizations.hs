{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Optimizations where

import Prelude (($), Either(..), String, (++), Show, elem, show, unwords, foldl, map, Bool(..), Maybe(..), null, (!!), any, (&&), (+), (-), unlines, reverse, div, mod, (*), not, (==), head, Eq, length, (/=), return, (||), Int, otherwise, error, Integer, (<), (<=), (>), (>=))
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map

data BasicBlock = BasicBlock
  { blockName :: String,
    statements :: [Stmt],
    successors :: [String],
    predecessors :: [String]
  }
  
optimizeProgram :: Program -> Program
optimizeProgram (Program pos topDefs) = 
  let optimizedTopDefs = map optimizeTopDef topDefs
      smth = generateCFG optimizedTopDefs
  in Program pos topDefs

optimizeTopDef :: TopDef -> TopDef
optimizeTopDef (FnDef pos retType name args block) =
  let optimizedBlock = optimizeBlock block
  in FnDef pos retType name args optimizedBlock

optimizeBlock :: Block -> Block
optimizeBlock (Block pos stmts) =
  Block pos (removeDeadCodeAfterReturn stmts)

removeDeadCodeAfterReturn :: [Stmt] -> [Stmt]
removeDeadCodeAfterReturn stmts = go stmts False
  where
    go [] _ = []
    go (stmt:rest) hasReturned
        | hasReturned = [] 
        | otherwise = case stmt of
            Ret _ _       -> stmt : go rest True 
            VRet _        -> stmt : go rest True 
            BStmt pos blk -> BStmt pos (removeDeadCodeFromBlock blk) : go rest hasReturned
            Cond pos cond trueStmt -> Cond pos cond (head (go [trueStmt] False)) : go rest hasReturned
            CondElse pos cond trueStmt falseStmt ->
                CondElse pos cond
                    (head (go [trueStmt] False))
                    (head (go [falseStmt] False)) : go rest hasReturned
            While pos cond body -> While pos cond (head (go [body] False)) : go rest hasReturned
            _ -> stmt : go rest hasReturned

    removeDeadCodeFromBlock :: Block -> Block
    removeDeadCodeFromBlock (Block pos stmts) =
        Block pos (go stmts False)

generateCFG :: [TopDef] -> [TopDef]
generateCFG topDefs =
    let (basicBlocksMap, _) = divideIntoBlocks topDefs
        fullBlocksMap = computePredecessors basicBlocksMap
    in topDefs

divideIntoBlocks :: [TopDef] -> (Map.Map String BasicBlock, Int)
divideIntoBlocks topDefs = 
    foldl processTopDef (Map.empty, 0) topDefs

processTopDef :: (Map.Map String BasicBlock, Int) -> TopDef -> (Map.Map String BasicBlock, Int)
processTopDef (blockMap, counter) (FnDef _ _ _ _ block) = 
    divideBlock block (blockMap, counter)

    -- Podział bloku
divideBlock :: Block -> (Map.Map String BasicBlock, Int) -> (Map.Map String BasicBlock, Int)
divideBlock (Block _ stmts) (blockMap, counter) = 
    foldl processStmt (blockMap, counter) stmts

    -- Przetwarzanie instrukcji
processStmt :: (Map.Map String BasicBlock, Int) -> Stmt -> (Map.Map String BasicBlock, Int)
processStmt (blockMap, counter) stmt = 
    let blockName = "L" ++ show counter
        currentBlock = Map.findWithDefault (BasicBlock blockName [] [] []) blockName blockMap
        newBlock = currentBlock { statements = statements currentBlock ++ [stmt] }
        newMap = Map.insert blockName newBlock blockMap
    in case stmt of
        -- Nowe bloki dla warunków i pętli
        Cond _ _ trueStmt ->
            let trueBlockName = "L" ++ show (counter + 1)
                -- Podział bloku trueStmt
                (mapAfterTrue, nextCounter) = divideBlock (Block Nothing [trueStmt]) (newMap, counter + 1)
                -- Dodanie prawdziwego i następnego bloku jako sukcesora bloku trueStmt
                nextBlockName = "L" ++ show nextCounter
                updatedBlock = newBlock { successors = successors newBlock ++ [trueBlockName, nextBlockName] }
                updatedMap = Map.insert blockName updatedBlock mapAfterTrue
            in (updatedMap, nextCounter)
        CondElse _ _ trueStmt falseStmt ->
            let trueBlockName = "L" ++ show (counter + 1)
                -- Podział bloku trueStmt
                (mapAfterTrue, nextCounter) = divideBlock (Block Nothing [trueStmt]) (newMap, counter + 1)
                falseBlockName = "L" ++ show nextCounter
                (mapAfterFalse, finalCounter) = divideBlock (Block Nothing [trueStmt]) (mapAfterTrue, nextCounter + 1)
                -- Dodanie prawdziwego i następnego bloku jako sukcesora bloku trueStmt
                nextBlockName = "L" ++ show finalCounter
                updatedBlock = newBlock { successors = successors newBlock ++ [trueBlockName, falseBlockName, nextBlockName] }
                updatedMap = Map.insert blockName updatedBlock mapAfterFalse
            in (updatedMap, finalCounter)
        While _ _ bodyStmt ->
            let conditionBlockName = blockName  -- Warunek pętli jest w tym samym bloku
                loopBodyName = "L" ++ show (counter + 1)  -- Blok dla ciała pętli
                (mapAfterLoopBody, nextCounter) = divideBlock (Block Nothing [bodyStmt]) (newMap, counter + 1)
                loopBodyBlock = Map.findWithDefault (BasicBlock loopBodyName [] [] []) loopBodyName mapAfterLoopBody
                updatedLoopBodyBlock = loopBodyBlock { successors = successors loopBodyBlock ++ [conditionBlockName] }
                mapWithUpdatedLoopBody  = Map.insert loopBodyName updatedLoopBodyBlock mapAfterLoopBody
                -- Dodajemy następnika po pętli do warunku pętli
                nextBlockName = "L" ++ show nextCounter
                updatedConditionBlock = newBlock { successors = successors newBlock ++ [loopBodyName, nextBlockName] }
                updatedMap = Map.insert conditionBlockName updatedConditionBlock mapAfterLoopBody
            in (updatedMap, nextCounter)
        -- Dla pozostałych instrukcji pozostajemy w tym samym bloku
        _ -> (newMap, counter)

computePredecessors :: Map.Map String BasicBlock -> Map.Map String BasicBlock
computePredecessors basicBlocksMap = 
    Map.mapWithKey updatePredecessors basicBlocksMap
  where
    -- Funkcja, która dla każdego bloku dodaje jego poprzedników
    updatePredecessors :: String -> BasicBlock -> BasicBlock
    updatePredecessors blockName block = 
        let updatedBlock = block { predecessors = findPredecessors blockName basicBlocksMap }
        in updatedBlock

    -- Funkcja znajdująca poprzedników dla danego bloku
    findPredecessors :: String -> Map.Map String BasicBlock -> [String]
    findPredecessors blockName blocksMap =
        [ predecessor | (predecessor, successorBlock) <- Map.toList blocksMap,
                       blockName `elem` successors successorBlock]