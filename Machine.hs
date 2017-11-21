module Machine where

import CodeConvert
import DeBruijnConvert
import Assign3
import Data.List
import Data.Maybe
import Data.Eq

type Machine = ([Code],[Stack],[Stack]) 


prettyMachine:: Machine -> [String]
prettyMachine mach = processStack stk [] where
    stk = comp mach


--computation function, (c,e,s) = (code, environment, stack)
comp:: Machine -> [Stack]
comp (c, e, s) = case ((c == []) && (e == [] )) of
    True -> s
    False -> comp (step (c,e,s))


step:: Machine -> Machine
step (Clo(c'):c,e,s) = (c,e, (SClos(c',e)):s)
step (CApp:c ,e, (SClos(c',e'):v:s)) = (c', v:e', SClos(c,e):s)
step ((CAccess n):c,e,s) = (c,e,(e !! (n-1)):s) 
step (CRet:c, e, v:(SClos(c',e'):s)) = (c',e',v:s)
step ((CConst k):c, e, s) = (c, e, (SInt k):s)
step (CAdd:c, e, (SInt n):(SInt m):s) = (c, e, (SInt(n + m)):s)
step (CMul:c, e, (SInt n):(SInt m):s) = (c, e, (SInt(n * m)):s)
step (CLeq:c, e, (SInt n):(SInt m):s) = (c, e, (SBoolean (n <= m)):s)
step ((CBoolean True):c, e, s) = (c, e, (SBoolean True):s)
step ((CBoolean False):c, e, s) = (c, e, (SBoolean False):s)
step ((CIf(c1,c2)):c, e, (SBoolean True):s) = (c1, e, (SClos(c,e)):s)
step ((CIf(c1,c2)):c, e, (SBoolean False):s) = (c2, e, (SClos(c,e)):s)
step (CNil:c, e, s) = (c,e, SNil:s)
step ((CCons):c, e, v1:v2:s) = (c,e, (SCons(v1,v2)):s)
step ((CCase(c1,c2):c, e, (SCons(v1,v2)):s)) = (c1, v1:v2:e, (SClos(c,e)):s)
step ((CCase(c1,c2)):c, e, (SNil:s)) = (c2, e, (SClos(c,e)):s)


--for Pretty Printing
processStack:: [Stack] -> [String] -> [String]
processStack [] strs = strs
processStack (s:stack) strs = case (s) of   
    SCons (a, SNil) -> (processStack [a] strs) ++ (processStack stack strs)
    SCons (a,b) -> (processStack [a] strs) ++ (processStack [b] strs) ++ (processStack stack strs)
    x -> [(show x)] ++ (processStack stack strs)
    
