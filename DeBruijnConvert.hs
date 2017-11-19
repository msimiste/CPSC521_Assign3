module DeBruijnConvert where

import Assign3
import Data.List
import Data.Maybe

debConv:: Eq a => Lam a -> Deb a
debConv l = debConv' [] l 

debConv':: Eq a => [a] -> Lam a -> Deb a
debConv' as (LAbst a l) = DAbst (debConv' (a:as) l) 
debConv' as (LVar a) = case (myLookup a as) of
    -1 -> error "Unbound var"
    num -> DVar num
  
myLookup:: Eq a => a -> [a] -> Int
myLookup a la = case (a `elem` la) of 
    True -> fromJust (elemIndex a la)
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
