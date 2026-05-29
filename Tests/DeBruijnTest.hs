module Tests.DeBruijnTest (deBruijnTests) where

import Tests.Grader
import Lambda (Lambda(..))
import qualified Lambda as L
import DeBruijn
import Default (vx, vy, vz, m, k, ki)

deBruijnTests :: TestCase ()
deBruijnTests = do
    toFromTests
    isNormalFormTests
    reduceTests
    stepTests
    pipelineTests

db :: Lambda -> DeBruijn
db = toDB []

toFromTests :: TestCase ()
toFromTests = group "4.1 / 4.2 toDB / fromDB" $ do
    assertEq "free var"          0.5 (DBFree "x")              (toDB [] vx)
    assertEq "bound var"         0.5 (DBVar 0)                 (toDB ["x"] vx)
    assertEq "bound depth 1"     0.5 (DBVar 1)                 (toDB ["y","x"] vx)
    assertEq "λx.x body"         0.5 (DBAbs "x" (DBVar 0))     (toDB [] (Abs "x" vx))
    assertEq "K combinator"      1.0 (DBAbs "x" (DBAbs "y" (DBVar 1)))
                                     (toDB [] k)
    assertEq "(x y)"             0.5 (DBApp (DBFree "x") (DBFree "y"))
                                     (toDB [] (App vx vy))

    assertEq "fromDB free"       0.5 vx              (fromDB [] (DBFree "x"))
    assertEq "fromDB bound"      0.5 vx              (fromDB ["x"] (DBVar 0))
    assertEq "fromDB λx.x"       0.5 (Abs "x" vx)    (fromDB [] (DBAbs "x" (DBVar 0)))
    assertEq "fromDB K"          0.5 k
        (fromDB [] (DBAbs "x" (DBAbs "y" (DBVar 1))))

    let rt e = fromDB [] (toDB [] e)
    assertEq "roundtrip I"       0.5 (Abs "x" vx)               (rt (Abs "x" vx))
    assertEq "roundtrip K"       0.5 k                          (rt k)
    assertEq "roundtrip ((I) y)" 0.5 (App (Abs "x" vx) vy)      (rt (App (Abs "x" vx) vy))

isNormalFormTests :: TestCase ()
isNormalFormTests = group "4.3 isNormalForm (DeBruijn)" $ do
    assert "free var"          0.5 (isNormalForm (DBFree "x"))
    assert "λ no redex"        0.5 (isNormalForm (DBAbs "x" (DBVar 0)))
    assert "redex at top"      0.5 (not $ isNormalForm
        (DBApp (DBAbs "x" (DBVar 0)) (DBFree "y")))
    assert "redex nested"      0.5 (not $ isNormalForm
        (DBApp (DBFree "f") (DBApp (DBAbs "x" (DBVar 0)) (DBFree "y"))))
    assert "redex under λ"     0.5 (not $ isNormalForm
        (DBAbs "z" (DBApp (DBAbs "x" (DBVar 0)) (DBFree "y"))))

reduceTests :: TestCase ()
reduceTests = group "4.4 reduce (DeBruijn, depth-aware)" $ do
    assertEq "subst at 0"      0.5 (DBFree "a")         (reduce (DBFree "a") (DBVar 0))
    assertEq "shift outer"     0.5 (DBVar 0)            (reduce (DBFree "a") (DBVar 1))
    assertEq "K applied: outer var picked up" 1.5
        (DBAbs "y" (DBFree "a"))
        (reduce (DBFree "a") (DBAbs "y" (DBVar 1)))
    assertEq "inner bound preserved" 1.0
        (DBAbs "y" (DBVar 0))
        (reduce (DBFree "a") (DBAbs "y" (DBVar 0)))

stepTests :: TestCase ()
stepTests = group "4.5 / 4.6 normalStep / applicativeStep (DeBruijn)" $ do
    assertEq "β at top"        1.0 (DBFree "y")
        (normalStep (DBApp (DBAbs "x" (DBVar 0)) (DBFree "y")))
    assertEq "leftmost first"  1.0
        (DBApp (DBAbs "y" (DBVar 0)) (DBFree "z"))
        (normalStep (DBApp (DBApp (DBAbs "x" (DBVar 0)) (DBAbs "y" (DBVar 0))) (DBFree "z")))
    assertEq "step under λ"    0.5
        (DBAbs "x" (DBFree "z"))
        (normalStep (DBAbs "x" (DBApp (DBAbs "y" (DBVar 0)) (DBFree "z"))))
    assertEq "applicative β when arg NF" 1.0 (DBFree "y")
        (applicativeStep (DBApp (DBAbs "x" (DBVar 0)) (DBFree "y")))
    assertEq "applicative reduces arg first" 1.0
        (DBApp (DBAbs "x" (DBVar 0)) (DBFree "z"))
        (applicativeStep (DBApp (DBAbs "x" (DBVar 0)) (DBApp (DBAbs "y" (DBVar 0)) (DBFree "z"))))

pipelineTests :: TestCase ()
pipelineTests = group "4.7 normal / applicative (DeBruijn)" $ do
    let ident_y = App (Abs "x" vx) vy
    assertEq "((\\x.x) y)"              0.5 (DBFree "y")  (last (normal (db ident_y)))
    let kab = App (App k vx) vy
    assertEq "((K x) y)"                1.0 (DBFree "x")  (last (normal (db kab)))
    let omega = App m m
    assertEq "Ω steps unbounded (sample 5)" 0.5 5
        (length (take 5 (normal (db omega))))
    assertEq "applicative ((\\x.x) y)"  0.5 (DBFree "y")  (last (applicative (db ident_y)))
  where
    _ = (ki, vz, L.normal)
