Mike Simister 10095107

Assignment 3

There are several test cases which are listed below. These test cases are included in the file Assign3.hs
All values are returned as a list of strings.

Included is a separate executable file called factorialTest.
This file caclulates the factorial of an integer. 

Limitations:

1. As it requires and returns an Int, factorialTest may be unreliable for values which 
return larger than an Int (larger than 2^31).

2. factorialTest will incorrectly return 1 if given an negative value as input.
This is the result of the data type being constrained by (<=) instead of (==)

To run factorial test
> ./factorialTest <integer>

To run an included test:

>ghci
Prelude>:l LambdaStart.hs
*LambdaStart> lambdaStart <testName>

example:
Prelude>:l LambdaStart.hs
*LambdaStart> lambdaStart ifTrueTest
["SInt 9"]

To run a non-included test:

1. Create a lambda term based on the Lam data type included in Assign3.hs

>ghci
Prelude> :l LambdaStart.hs
*LambdaStart> lambdaStart <lambda term>


/***************Included Tests and Functions**********************/
The tests and functions below are included in Assign3.hs


--constants
fixpoint = (LAbst "f" (LApp(LAbst "a" (LApp (LVar "f")(LAbst "x" (LApp(LApp (LVar "a")(LVar "a"))(LVar "x")))))(LAbst "a" (LApp (LVar "f")(LAbst "x" (LApp(LApp (LVar "a")(LVar "a"))(LVar "x"))))))) 
omega = LApp (LAbst "x" (LApp (LVar "x")(LVar "x")))(LAbst "x" (LApp(LVar "x")(LVar "x")))
factorialTest = LApp(LApp(fixpoint)(m_factorial))

--Functions:
m_factorial = (LAbst "f" (LAbst "n" ((LIf(Leq(LVar "n")(LConst 0))(LConst 1)(LMul(LVar "n")(LApp(LVar "f")(LAdd(LVar "n")(LConst (-1)))))))))

--tests

test1 = LApp (LAbst "x" (LApp (LVar "x")(LVar "x"))) (LAbst "x"(LVar"x"))

test2 = LApp (LApp (LAbst "x" (LAbst "y" (LApp (LVar"x")(LVar "y")))) (LAbst "x" (LVar "x"))) (LAbst "y" (LVar "y"))

test3 = LApp (LAbst "x" (LAdd (LVar "x" ) (LConst 1))) (LConst 2)

ifTrueTest = LApp (LAbst "x" ((LIf (LBoolean True)(LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x")))))(LConst 3)

ifFalseTest = LApp (LAbst "x" ((LIf (LBoolean False)(LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x")))))(LConst 3)

caseTest = LApp(LAbst "x" (LCase (LVar "x") (LMul(LVar "x")(LVar "x"))(LAdd(LVar "x")(LVar "x"))))(LCons(LConst 4)(LNil))

