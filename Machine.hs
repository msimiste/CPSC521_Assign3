module Machine where

import CodeConvert
import Assign3
import Data.List
import Data.Maybe
import Data.Eq

ntype Machine = Machine ([Code],[Stack],[Stack]) deriving (Eq)




--computation function, (c,e,s) = (code, environment, stack)
comp::Eq a => Machine -> [Stack]
comp Machine (c,e,s) = case (c == [] && e == [] ) of
    True -> s
    False -> comp (step (c,e,s))


step:: Machine -> Machine
step (Clo(c'):c,e,s) = (c,e, (SClos(c',e)):s)
step (CApp:c ,e, (SClos(c',e'):v:s)) = (c', v:e', SClos(c,e):s)
step ((CAccess n):c,e,s) = (c,e,(e !! n):s) 
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
step ((CCase(c1,c2):c, e, (SCons(v1,v2)):s)) = (c1, v1:v2:e, (SClos(c,e)):s)
step ((CCase(c1,c2)):c, e, (SNil:s)) = (c2, e, (SClos(c,e)):s)
