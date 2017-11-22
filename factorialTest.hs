module FactorialTest where

import DeBruijnConvert
import CodeConvert
import Machine
import Assign3
import Data.List
import Data.Maybe
import System.Environment


main = do
    args <- getArgs
    let input = args !! 0
    let lamFcn = factorialTest(LConst (read input))
    let dConv = debConv lamFcn
    let cCode = codeConv dConv
    let out = prettyMachine (cCode,[],[])    
    putStrLn(show out)
