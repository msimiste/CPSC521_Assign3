module DeBruijnConvert where

import Assign3

debConv:: Eq a => Lam a -> Deb
debConv l = dconv' [] l where

debConv':: Eq a => [a] -> Lam a -> Deb a
debConv' as (LAbst a l) = DAbst (debConv' (a:as) l) 
debConv' as LVar a = case (lookup a as) of
    Nothing -> error "Unbound var"
    Just a -> DVar a
    
            
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
