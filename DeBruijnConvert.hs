module DeBruijnConvert where

import Assign3
import Data.List
import Data.Maybe

debConv:: Eq a => Lam a -> Deb a
debConv l = debConv' [] l 

debConv':: Eq a => [a] -> Lam a -> Deb a
debConv' as (LAbst a l) = DAbst a (debConv' (a:as) l) 
debConv' as (LVar a) = case (myLookup a as) of
    -1 -> error "Unbound var"
    num -> DVar num
debConv' as  (LApp l1 l2) = DApp (debConv' as l1) (debConv' as l2)
debConv' as (LConst a)  = DConst a
debConv' as (LAdd l1 l2) = DAdd (debConv' as l1) (debConv' as l2)
debConv' as (LMul l1 l2) = DMul (debConv' as l1) (debConv' as l2)
debConv' as (Leq  l1 l2) = Deq (debConv' as l1) (debConv' as l2)
debConv' as (LBoolean bol) = DBoolean bol
debConv' as (LIf l1 l2 l3) = DIf (debConv' as l1) (debConv' as l2) (debConv' as l3)
debConv' as (LCons l1 l2) = DCons (debConv' as l1) (debConv' as l2)
debConv' as (LNil) = DNil
debConv' as (LCase l1 l2 l3) = DCase (debConv' as l1) (debConv' as l2) (debConv' as l3)
  
myLookup:: Eq a => a -> [a] -> Int
myLookup a la = case (a `elem` la) of 
    True -> (fromJust (elemIndex a la))+1
    False -> -1


--trans:: Deb -> [Code]
--trans DAbst l = [Clo(trans l) ++ (Ret)]

--Lambda Terms | [Code]
--{\x.t}v      | Clo ({t}x:v ++ Ret)



--data Stack = SInt Int
--        | SBoolean Bool
--        | SNil 
--        | SClos ([Code],[Stack])
          
--        
--type Machine = ([Code],[Stack],[Stack])
--
--step:: Machine -> Machine
--step (Clo(c'):c,e,s) = (c,e, (SClos(c',e)):s)
--
--
----computation function, (c,e,s) = (code, environment, stack)
--comp:: Machine -> [Stack]
--comp (c,e,s) = case (c == [] && e == [] ) of
--    True -> s
--    False -> comp (step (c,e,s))
--    
--transition table
--
--
--            before                                  after
--code        |   environment     | stack      code        | environment      | stack 
--Clo(c'):c           e               s        c              e                   (SCLos(c'e):S)
--
