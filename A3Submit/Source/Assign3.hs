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
        | LCase (Lam a) (Lam a) (Lam a) deriving (Eq, Show, Read)
    
data Deb  = DVar Int
        | DAbst (Deb)
        | DApp (Deb) (Deb)
        | DConst Int --should be DConst?
        | DAdd (Deb) (Deb)  
        | DMul (Deb) (Deb)
        | Deq (Deb) (Deb)
        | DIf (Deb) (Deb) (Deb)
        | DBoolean Bool
        | DCons (Deb) (Deb)
        | DNil
        | DCase (Deb) (Deb) (Deb) deriving (Eq,Show, Read)
        
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
        | CCons
        | CCase ([Code],[Code])
        | CIf ([Code],[Code]) deriving (Eq, Show)
        
data Stack = SInt Int
        | SBoolean Bool
        | SNil 
        | SCons (Stack, Stack)
        | SClos ([Code],[Stack]) deriving (Eq,Show)

--fixPoint = (\f.(\a.f(\x.aax))(\a.f(\x.aax)))
fixpoint = (LAbst "f" (LApp(LAbst "a" (LApp (LVar "f")(LAbst "x" (LApp(LApp (LVar "a")(LVar "a"))(LVar "x")))))(LAbst "a" (LApp (LVar "f")(LAbst "x" (LApp(LApp (LVar "a")(LVar "a"))(LVar "x"))))))) 

--factorial lambdaFunction
m_factorial = (LAbst "f" (LAbst "n" ((LIf(Leq(LVar "n")(LConst 0))(LConst 1)(LMul(LVar "n")(LApp(LVar "f")(LAdd(LVar "n")(LConst (-1)))))))))

factorialTest = LApp(LApp(fixpoint)(m_factorial))

--(\x.xx)(\x.x)
test1 = LApp (LAbst "x" (LApp (LVar "x")(LVar "x"))) (LAbst "x"(LVar"x"))

--(\xy.xy)(\x.x)(\y.y)
test2 = LApp (LApp (LAbst "x" (LAbst "y" (LApp (LVar"x")(LVar "y")))) (LAbst "x" (LVar "x"))) (LAbst "y" (LVar "y"))
--(\x.x +1)2
test3 = LApp (LAbst "x" (LAdd (LVar "x" ) (LConst 1))) (LConst 2)

--ifTest
ifTrueTest = LApp (LAbst "x" ((LIf (LBoolean True)(LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x")))))(LConst 3)

--ifTest
ifFalseTest = LApp (LAbst "x" ((LIf (LBoolean False)(LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x")))))(LConst 3)

--caseTest
caseTest = LApp(LAbst "x" (LCase (LVar "x") (LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x"))))(LCons(LConst 4)(LNil))

omega = LApp (LAbst "x" (LApp (LVar "x")(LVar "x")))(LAbst "x" (LApp(LVar "x")(LVar "x")))















