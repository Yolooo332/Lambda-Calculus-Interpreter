module Tests.ParserTest (parserTests) where

import Tests.Grader
import Lambda
import Parser
import Default

parserTests :: TestCase ()
parserTests = parseLambdaTests

parseLambdaTests :: TestCase ()
parseLambdaTests = group "2.1 parseLambda" $ do
    assertEq "variable"                 1.0 vx                          (parseLambda "x")
    assertEq "multichar variable"       1.0 (Var "alonzo")              (parseLambda "alonzo")
    assertEq "application"              1.0 (App vx vy)                 (parseLambda "(x y)")
    assertEq "left-nested application"  1.0 (App (App vx vy) vz)        (parseLambda "((x y) z)")
    assertEq "right-nested application" 1.0 (App vx (App vy vz))        (parseLambda "(x (y z))")
    assertEq "abstraction"              1.0 (Abs "x" vy)                (parseLambda "\\x.y")
    assertEq "two-abs application"      1.0 (App (Abs "x" vy) i)        (parseLambda "((\\x.y) (\\x.x))")
    assertEq "K KI"                     1.0 (App k ki)                  (parseLambda "((\\x.\\y.x) (\\x.\\y.y))")
    assertEq "M-combinator"             1.5 m                           (parseLambda "\\x.(x x)")
    assertEq "I-combinator"             1.5 i                           (parseLambda "\\x.x")
    assertEq "K-combinator"             1.5 k                           (parseLambda "\\x.\\y.x")
    assertEq "KI-combinator"            1.5 ki                          (parseLambda "\\x.\\y.y")
    assertEq "C-combinator"             3.0 c                           (parseLambda "\\x.\\y.\\z.((x z) y)")
    assertEq "Y-combinator"             3.0 y                           (parseLambda "\\f.((\\x.(f (x x))) (\\x.(f (x x))))")
