module Main where

import DeBruijnConvert
import CodeConvert
import Machine
import Assign3
import Data.List
import Data.Maybe
import System.Environment

main = do
    args <- getArgs
    let fname = args !! 0
    test <- readFile fname
    let dConv = (debConv (Lam test))
    let cCode = codeConv dConv 
    putStrLn  (prettyMachine (cCode , [], []))
 
