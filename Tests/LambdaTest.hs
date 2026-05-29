module Tests.LambdaTest (lambdaTests) where

import Data.List (sort, (\\))

import Tests.Grader
import Lambda
import Default

lambdaTests :: TestCase ()
lambdaTests = do
    eqTests
    varsTests
    freeVarsTests
    newVarTests
    isNormalFormTests
    reduceTests
    normalStepTests
    applicativeStepTests
    simplifyTests

eqTests :: TestCase ()
eqTests = group "0. Eq sanity (free vars are wildcards)" $ do
    assertEq    "x == x"            0   (Var "x")           (Var "x")
    assertEq    "x == y"            0   (Var "x")           (Var "y")
    assertEq    "λx.x == λx.x"      0   (Abs "x" vx)        (Abs "x" vx)
    assertEq    "λx.x == λy.y"      0   (Abs "x" vx)        (Abs "y" vy)
    assertNeq   "λx.x != λx.y"      0   (Abs "x" vx)        (Abs "x" vy)
    assertEq    "(x y) == (n m)"    0   (App vx vy)         (App vn vm)

varsTests :: TestCase ()
varsTests = group "1.1 vars" $ do
    assertEq "vars x"           0.5 (sort ["x"])      (sort $ vars vx)
    assertEq "vars (λx.x)"      0.5 (sort ["x"])      (sort $ vars (Abs "x" vx))
    assertEq "vars (λx.y)"      0.5 (sort ["x","y"])  (sort $ vars (Abs "x" vy))
    assertEq "vars (x y)"       0.5 (sort ["x","y"])  (sort $ vars (App vx vy))
    assertEq "vars (λx.(x y))"  0.5 (sort ["x","y"])  (sort $ vars (Abs "x" (App vx vy)))

freeVarsTests :: TestCase ()
freeVarsTests = group "1.2 freeVars" $ do
    assertEq "freeVars x"        0.5 (sort ["x"])      (sort $ freeVars vx)
    assertEq "freeVars (λx.x)"   0.5 []                (freeVars (Abs "x" vx))
    assertEq "freeVars (λx.y)"   0.5 (sort ["y"])      (sort $ freeVars (Abs "x" vy))
    assertEq "freeVars (x y)"    0.5 (sort ["x","y"])  (sort $ freeVars (App vx vy))
    let e = App (Abs "x" vz) (App vx vz)
    assertEq "freeVars complex"  0.5 (sort ["x","z"])  (sort $ freeVars e)
    assertEq "freeVars Y"        0.5 []                (freeVars y)

newVarTests :: TestCase ()
newVarTests = group "1.3 newVar" $ do
    let alphabet   = map (:[]) ['a'..'z']
    let alphabet_2 = [a ++ b | a <- alphabet, b <- "":alphabet]
    assertEq "newVar [a]"             0.5 "b"   (newVar ["a"])
    assertEq "newVar [a..e]"          0.5 "f"   (newVar ["a","b","c","d","e"])
    assertEq "newVar 1-char taken"    1.0 "aa"  (newVar alphabet)
    assertEq "newVar 1+2-char taken"  1.0 "aaa" (newVar alphabet_2)
    assertEq "newVar fills first gap" 1.5 "cd"  (newVar (alphabet_2 \\ ["cd"]))
    assertEq "newVar earliest gap"    2.5 "ab"  (newVar (alphabet_2 \\ ["cd","ab"]))

isNormalFormTests :: TestCase ()
isNormalFormTests = group "1.4 isNormalForm" $ do
    assert "x"                  0.25 (isNormalForm vx)
    assert "λx.x"               0.25 (isNormalForm i)
    assert "(x y)"              0.25 (isNormalForm (App vx vy))
    assert "λx.(x y)"           0.25 (isNormalForm (Abs "x" (App vx vy)))
    assert "(λx.(x x)) x"       0.5  (not $ isNormalForm $ App m vx)
    assert "Ω is not NF"        0.5  (not $ isNormalForm $ App m m)
    assert "Y is not NF"        0.5  (not $ isNormalForm y)
    assert "redex under λ"      0.5  (not $ isNormalForm $ Abs "x" $ App m vx)

reduceTests :: TestCase ()
reduceTests = group "1.5 reduce" $ do
    assertEq    "subst hits target"     1.0 m                            (reduce "x" vx m)
    assertEq    "subst misses free"     1.0 vy                           (reduce "x" vy m)
    assertEq    "subst inside app"      1.0 (App m m)                    (reduce "x" (App vx vx) m)
    assertEq    "subst under λ"         1.0 (Abs "y" m)                  (reduce "x" (Abs "y" vx) m)
    assertEq    "shadowed by λ"         1.0 (Abs "x" vx)                 (reduce "x" (Abs "x" vx) m)
    let e1 = Abs "y" (App vx vy)
    let e2 = Abs "x" vy
    assertEq    "capture avoidance"       3.0 (Abs "a" (App (Abs "x" vy) (Var "a")))
                                              (reduce "x" e1 e2)
    assertEq    "reduce hits binder name" 3.0 (Abs "x" (App vy vx))
                                              (reduce "y" e1 e2)

expr1 :: Lambda
expr1 = App (App k vx) (App ki vy)

normalAns1 :: [Lambda]
normalAns1 =
    [ expr1
    , App (Abs "y" vx) (App ki vy)
    , vx
    ]

applicativeAns1 :: [Lambda]
applicativeAns1 =
    [ expr1
    , App (Abs "y" vx) (App ki vy)
    , App (Abs "y" vx) (Abs "z" vz)
    , vx
    ]

normalStepTests :: TestCase ()
normalStepTests = group "1.6 normalStep" $ do
    assertEq "normalStep on var (no-op)"      0.5 vx           (normalStep vx)
    assertEq "normalStep redex"               0.5 (App vx vx)  (normalStep (App m vx))
    assertEq "normalStep skips diverging arg" 1.0 vy
        (normalStep $ App (Abs "x" vy) (App m m))
    assertEq "normalStep step 1"              1.0 (normalAns1 !! 1) (normalStep expr1)
    assertEq "normalStep step 2"              1.0 (normalAns1 !! 2) (normalStep (normalStep expr1))
    let y1 = case y of
                 Abs _ body -> Abs "f" (App vf body)
                 _          -> error "Default.y is no longer an Abs"
    assertEq "normalStep on Y"                1.0 y1 (normalStep y)

applicativeStepTests :: TestCase ()
applicativeStepTests = group "1.7 applicativeStep" $ do
    assertEq "applicativeStep var"        0.5 vx          (applicativeStep vx)
    assertEq "applicativeStep redex"      0.5 (App vx vx) (applicativeStep (App m vx))
    let stuck = App (Abs "x" vy) (App m m)
    assertEq "applicative gets stuck on Ω" 1.0 stuck       (applicativeStep stuck)
    assertEq "applicativeStep step 1" 1.0 (applicativeAns1 !! 1) (applicativeStep expr1)
    assertEq "applicativeStep step 2" 1.0 (applicativeAns1 !! 2) (applicativeStep (applicativeStep expr1))
    assertEq "applicativeStep step 3" 1.0 (applicativeAns1 !! 3)
        (applicativeStep (applicativeStep (applicativeStep expr1)))

simplifyTests :: TestCase ()
simplifyTests = group "1.8 simplify / normal / applicative" $ do
    assertEq "simplify on NF"         0.5 [c]               (simplify id c)
    assertEq "normal order chain"     1.0 normalAns1        (simplify normalStep expr1)
    assertEq "applicative chain"      1.0 applicativeAns1   (simplify applicativeStep expr1)
    assertEq "normal == default"      0.5 (normal expr1)        (simplify normalStep expr1)
    assertEq "applicative == default" 0.5 (applicative expr1) (simplify applicativeStep expr1)
