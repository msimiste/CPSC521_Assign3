module CodeConvert where

import DeBruijnConvert
import Assign3
import Data.List
import Data.Maybe


codeConv:: Deb -> [Code]
codeConv deb = trans [] deb

trans:: [Code] -> Deb -> [Code]
trans as (DVar num) = as ++ [CAccess num]
trans as (DAbst l) = as ++ [(Clo ((trans as l) ++ [CRet]))]
trans as (DApp d1 d2) = as ++ ((trans as d2) ++ (trans as d1)) ++ [CApp]
trans as (DConst num) = as ++ [CConst num]
trans as (DAdd d1 d2) = as ++ ((trans as d2) ++ (trans as d1)) ++ [CAdd]
trans as (DMul d1 d2)  = as ++ ((trans as d2) ++ (trans as d1)) ++ [CMul]
trans as (Deq d1 d2) = as ++((trans as d2) ++ (trans as d1)) ++ [CLeq]
trans as (DIf d1 d2 d3) = as ++ ((trans as d1) ++ [CIf (((trans as d2) ++ [CRet]),((trans as d3) ++ [CRet]))])
trans as (DBoolean bool) = case (bool) of
    True -> as ++ [CBoolean True]
    False -> as ++ [CBoolean False]
trans as (DCons d1 d2) = as ++ (trans as d2) ++ (trans as d1) ++ [CCons]
trans as (DNil) = as ++ [CNil]
trans as (DCase d1 d2 d3) = as ++ (trans as d1) ++ [CCase(((trans as d2) ++ [CRet]),((trans as d3) ++ [CRet]))]

