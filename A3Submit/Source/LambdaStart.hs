
module LambdaStart where

import DeBruijnConvert
import CodeConvert
import Machine
import Assign3
import Data.List
import Data.Maybe
import System.Environment


lambdaStart:: Eq a => Lam a -> [String]
lambdaStart lam = out where
    dConv = debConv lam
    cCode = codeConv dConv
    out = prettyMachine (cCode, [], [])
