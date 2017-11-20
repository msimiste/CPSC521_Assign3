module CodeConvert where

import DeBruijnConvert
import Data.List
import Data.Maybe


codeConv Eq a => Deb a -> [Code]
codeConv deb = trans [] deb

trans::Eq a => [a] -> Deb a -> [Code]
trans as (DVar num) = [CAccess num]
trans as (DAbst l) = [Clo[trans l] ++ [CRet]] ++ as
trans as (DApp d1 d2) =  
trans as (DConst num) = [CConst num ]
trans as (DAdd d1 d2) = 
trans as (DMul d1 d2  = 
trans as (Deq d1 d2) =
trans as (DIf d1 d2 d3) =
trans as (DBoolean bool) =
trans as (DCons d1 d2) =
trans as (DNil) =
trans as (DCase d1 d2 d3) =

