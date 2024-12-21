{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Optimizations where

import Prelude (($), IO, putStrLn, mapM_, Ord, Either(..), String, (++), Show, elem, show, unwords, foldl, map, Bool(..), Maybe(..), null, (!!), any, (&&), (+), (-), unlines, reverse, div, mod, (*), not, (==), head, Eq, length, (/=), return, (||), Int, otherwise, error, Integer, (<), (<=), (>), (>=))
import Control.Monad (foldM, mapM, forM_)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map

data BasicBlock = BasicBlock
  { blockName :: String,
    statements :: [Stmt],
    successors :: [String],
    predecessors :: [String]
  }

type BasicBlockMap = Map.Map String BasicBlock

optimizeProgram :: Program -> BasicBlockMap
optimizeProgram (Program pos topDefs) = 
  let optimizedTopDefs = map optimizeTopDef topDefs
      cfg = generateCFG optimizedTopDefs
  in cfg

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

generateCFG :: [TopDef] -> BasicBlockMap
generateCFG topDefs =
    let basicBlocksMap = divideIntoBlocks topDefs
        fullBlocksMap = computePredecessors basicBlocksMap
    in fullBlocksMap

printBasicBlockMap :: BasicBlockMap -> IO ()
printBasicBlockMap basicBlocksMap = do
    putStrLn "BasicBlockMap Contents:"
    forM_ (Map.toList basicBlocksMap) printBasicBlock

-- Funkcja do wypisywania zawartości pojedynczego BasicBlocka
printBasicBlock :: (String, BasicBlock) -> IO ()
printBasicBlock (blockName, block) = do
    putStrLn $ "Block Name: " ++ blockName
    putStrLn $ "Statements: " ++ show (statements block)
    putStrLn $ "Successors: " ++ show (successors block)
    putStrLn $ "Predecessors: " ++ show (predecessors block)
    putStrLn "----------------------------------"

divideIntoBlocks :: [TopDef] -> BasicBlockMap
divideIntoBlocks topDefs = 
    foldl processTopDef Map.empty topDefs

processTopDef :: BasicBlockMap -> TopDef -> BasicBlockMap
processTopDef blockMap (FnDef _ _ funcName _ block) = 
    let (blockMapNew, _) = divideBlock (getIdentName funcName) block (blockMap, 0)
    in blockMapNew

getIdentName :: Ident -> String
getIdentName (Ident name) = name

    -- Podział bloku
divideBlock :: String -> Block -> (BasicBlockMap, Int) -> (BasicBlockMap, Int)
divideBlock funcName (Block _ stmts) (blockMap, counter) = 
    foldl (processStmt funcName) (blockMap, counter) stmts

    -- Przetwarzanie instrukcji
processStmt :: String -> (BasicBlockMap, Int) -> Stmt -> (BasicBlockMap, Int)
processStmt funcName (blockMap, counter) stmt = 
        -- Nowe bloki dla warunków i pętli
    case stmt of 
        Cond _ condition trueStmt -> handleCond funcName blockMap (counter+1) condition trueStmt
        CondElse _ _ trueStmt falseStmt -> handleCondElse funcName blockMap (counter+1) trueStmt falseStmt
        While _ _ bodyStmt -> handleWhile funcName blockMap (counter+1) bodyStmt
        BStmt _ block -> divideBlock funcName block (blockMap, counter+1)
        -- Dla pozostałych instrukcji pozostajemy w tym samym bloku
        _ -> 
            let blockName = funcName ++ "_L" ++ show counter
                currentBlock = Map.findWithDefault (BasicBlock blockName [] [] []) blockName blockMap
                newBlock = currentBlock { statements = statements currentBlock ++ [stmt] }
                newMap = Map.insert blockName newBlock blockMap
            in (newMap, counter)

generateBlockName :: String -> Int -> String
generateBlockName funcName counter = funcName ++ "_L" ++ show counter

-- Obsługa instrukcji warunkowej (Cond)
handleCond :: String -> BasicBlockMap -> Int -> Expr -> Stmt -> (BasicBlockMap, Int)
handleCond funcName blockMap counter conditionExpr trueStmt = 
    let conditionBlockName = generateBlockName funcName counter
        prevBlockName = generateBlockName funcName (counter - 1)
        prevBlock = Map.findWithDefault (BasicBlock prevBlockName [] [] []) prevBlockName blockMap
        updPrevBlock = prevBlock { successors = successors prevBlock ++ [conditionBlockName] }
        
        trueBlockName = generateBlockName funcName (counter + 1)
        (mapAfterTrue, nextCounter) = divideBlock funcName (Block Nothing [trueStmt]) (blockMap, counter + 1)
        
        nextBlockName = generateBlockName funcName (nextCounter + 1)
        
        -- Aktualizujemy blok warunku
        updatedConditionBlock = BasicBlock conditionBlockName [Cond Nothing conditionExpr (BStmt Nothing (Block Nothing []))] [trueBlockName, nextBlockName] []
        -- Aktualizujemy blok ciała true
        updatedTrueBlock = updateBlockWithSuccessors mapAfterTrue trueBlockName [nextBlockName]
        
    -- Dodajemy do mapy bloki warunkowe i sukcesory
    in (insertMultiple [(prevBlockName, updPrevBlock), (trueBlockName, updatedTrueBlock), (conditionBlockName, updatedConditionBlock)] mapAfterTrue, nextCounter + 1)


insertMultiple :: Ord k => [(k, v)] -> Map.Map k v -> Map.Map k v
insertMultiple keyValuePairs map = foldl (\m (k, v) -> Map.insert k v m) map keyValuePairs

-- Obsługa instrukcji warunkowej z blokiem else (CondElse)
handleCondElse :: String -> BasicBlockMap -> Int -> Stmt -> Stmt -> (BasicBlockMap, Int)
handleCondElse funcName blockMap counter trueStmt falseStmt = 
    let conditionBlockName = generateBlockName funcName counter
        trueBlockName = generateBlockName funcName (counter + 1)
        (mapAfterTrue, nextCounter) = divideBlock funcName (Block Nothing [trueStmt]) (blockMap, counter + 1)
        
        falseBlockName = generateBlockName funcName (nextCounter + 1)
        (mapAfterFalse, finalCounter) = divideBlock funcName (Block Nothing [falseStmt]) (mapAfterTrue, nextCounter + 1)
        
        nextBlockName = generateBlockName funcName (finalCounter + 1)
        
        -- Aktualizujemy blok warunku
        updatedConditionBlock = updateBlockWithSuccessors mapAfterFalse conditionBlockName [trueBlockName, falseBlockName]
        -- Aktualizujemy blok ciała true
        updatedTrueBlock = updateBlockWithSuccessors mapAfterFalse trueBlockName [nextBlockName]
        -- Aktualizujemy blok ciała false
        updatedFalseBlock = updateBlockWithSuccessors mapAfterFalse falseBlockName [nextBlockName]
        
    -- Dodajemy do mapy bloki warunkowe oraz ich sukcesory
    in (insertMultiple [(trueBlockName, updatedTrueBlock), (falseBlockName, updatedFalseBlock), (conditionBlockName, updatedConditionBlock)] mapAfterTrue, finalCounter + 1)

-- Obsługa instrukcji pętli (While)
handleWhile :: String -> BasicBlockMap -> Int -> Stmt -> (BasicBlockMap, Int)
handleWhile funcName blockMap counter bodyStmt = 
    let conditionBlockName = generateBlockName funcName counter
        loopBodyName = generateBlockName funcName (counter + 1)
        (mapAfterLoopBody, nextCounter) = divideBlock funcName (Block Nothing [bodyStmt]) (blockMap, counter + 1)
        
        -- Aktualizujemy blok ciała pętli, aby wskazywał z powrotem na warunek
        loopBodyBlock = Map.findWithDefault (BasicBlock loopBodyName [] [] []) loopBodyName mapAfterLoopBody
        updatedLoopBodyBlock = loopBodyBlock { successors = successors loopBodyBlock ++ [conditionBlockName] }
        mapWithUpdatedLoopBody  = Map.insert loopBodyName updatedLoopBodyBlock mapAfterLoopBody
        
        nextBlockName = generateBlockName funcName (nextCounter + 1)
        
        -- Aktualizujemy blok warunku pętli
        updatedConditionBlock = updateBlockWithSuccessors blockMap conditionBlockName [loopBodyName, nextBlockName]
        
    -- Dodajemy do mapy blok warunku oraz ciała pętli
    in (Map.insert conditionBlockName updatedConditionBlock mapWithUpdatedLoopBody, nextCounter + 1)

-- Funkcja do zaktualizowania bloku z nowymi sukcesorami
updateBlockWithSuccessors :: BasicBlockMap -> String -> [String] -> BasicBlock
updateBlockWithSuccessors blockMap blockName newSuccessors = 
    let currentBlock = Map.findWithDefault (BasicBlock blockName [] [] []) blockName blockMap
    in currentBlock { successors = successors currentBlock ++ newSuccessors }

computePredecessors :: BasicBlockMap -> BasicBlockMap
computePredecessors basicBlocksMap = 
    Map.mapWithKey updatePredecessors basicBlocksMap
  where
    updatePredecessors :: String -> BasicBlock -> BasicBlock
    updatePredecessors blockName block = 
        let updatedBlock = block { predecessors = findPredecessors blockName basicBlocksMap }
        in updatedBlock

    -- Funkcja znajdująca poprzedników dla danego bloku
    findPredecessors :: String -> BasicBlockMap -> [String]
    findPredecessors blockName blocksMap =
        [ predecessor | (predecessor, successorBlock) <- Map.toList blocksMap,
                       blockName `elem` successors successorBlock]