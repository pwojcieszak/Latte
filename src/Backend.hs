{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE RankNTypes #-}

module Backend where

import Prelude (($), Either(..), String, (++), Show, show, unwords, foldl, map, Bool(..), Maybe(..), null, (!!), any, (&&), (+), (-), unlines, reverse, div, mod, (*), not, (==), head, Eq, length, (/=), return, (||), Int, otherwise, error, Integer, (<), (<=), (>), (>=))
import Control.Monad (foldM, mapM)
import qualified AbsLatte
import AbsLatte
import qualified Data.Map as Map

generateLLVM :: Program -> String
generateLLVM program =
  let codeBuffer = ["test","a"]
  in unlines (reverse codeBuffer)

