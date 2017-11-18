module Assign3 where


data Lam a = LVar a 
        | LAbst a (Lam a) 
        | LApp (Lam a) (Lam a)
        | LConst Int
        | LAdd (Lam a) (Lam a)
        | LMul (Lam a) (Lam a)
        | Leq (Lam a) (Lam a)
        | LBoolean Bool
        | LIf (Lam a) (Lam a) (Lam a)
        | LCons (Lam a) (Lam a)
        | LNil
        | LCase (Lam a) (Lam a) (Lam a) deriving Show
    
data Deb a = Dvar Int
        | DAbst (Deb a)
        | DApp (Deb a) (Deb a)
        | DConst
        | DAdd (Deb a) (Deb a)  
        | DMul (Deb a) (Deb a)
        | Deq (Deb a) (Deb a)
        | DIf (Deb a) (Deb a) (Deb a)
        | DBoolean Bool
        | DCons (Deb a) (Deb a)
        | DNil
        | DCase (Deb a) (Deb a) (Deb a) deriving Show
        
data Code =  Clo [Code]
        | CApp
        | CAccess Int
        | CRet
        | CConst Int
        | CAdd
        | CMul
        | CLeq
        | CBoolean Bool
        | CNil
        | CCase ([Code],[Code])
        | Cif ([Code],[Code)
        
data Stack = SInt Int
        | SBoolean Bool
        | SNil 
        | SClos ([Code],[Stack])
