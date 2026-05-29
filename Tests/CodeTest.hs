module Tests.CodeTest (codeTests) where

import Tests.Grader
import Lambda
import Parser
import Code
import Default

codeTests :: TestCase ()
codeTests = do
    simplifyCtxTests
    normalApplicativeCtxTests
    parseMacroTests
    parseLineTests

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

simplifyCtxTests :: TestCase ()
simplifyCtxTests = group "3.1 simplifyCtx" $ do
    assertEq    "var is NF"            1.0 (Right [vx])             (simplifyCtx [] normalStep vx)
    assertEq    "normal order chain"   1.5 (Right normalAns1)       (simplifyCtx [] normalStep expr1)
    assertEq    "applicative chain"    1.5 (Right applicativeAns1)  (simplifyCtx [] applicativeStep expr1)
    assertErr   "missing macro"        1.5 (simplifyCtx [("X", vx)] id (Macro "Y"))
    assertEq    "expand simple macro"  1.5 (Right [vx])             (simplifyCtx [("X", vx)] id (Macro "X"))
    let complexExpr = App (Macro "C") (App (Macro "K") (Macro "I"))
    let complexAns =
            [ App c (App k i)
            , Abs "y" $ Abs "z" $ App (App (App k i) vz) vy
            , Abs "y" $ Abs "z" $ App (App ki vz) vy
            , Abs "y" $ Abs "z" $ App i vy
            , k
            ]
    assertEq    "expand C (K I)"       5.0 (Right complexAns) (simplifyCtx defaultContext normalStep complexExpr)

normalApplicativeCtxTests :: TestCase ()
normalApplicativeCtxTests = group "3.1 normalCtx / applicativeCtx" $ do
    assertEq    "normalCtx == simplifyCtx normalStep" 0.5
        (simplifyCtx [] normalStep expr1)         (normalCtx [] expr1)
    assertEq    "applicativeCtx == simplifyCtx applicativeStep" 0.5
        (simplifyCtx [] applicativeStep expr1)    (applicativeCtx [] expr1)
    assertErr   "normalCtx missing macro"      0.5 (normalCtx [] (Macro "MISSING"))
    assertErr   "applicativeCtx missing macro" 0.5 (applicativeCtx [] (Macro "MISSING"))

parseMacroTests :: TestCase ()
parseMacroTests = group "3.2 parseLambda with macros" $ do
    assertEq "simple macro"      0.5 (Macro "X")                          (parseLambda "X")
    assertEq "multichar macro"   0.5 (Macro "CHURCH")                     (parseLambda "CHURCH")
    assertEq "macro with digits" 0.5 (Macro "420B3")                      (parseLambda "420B3")
    assertEq "AND TRUE FALSE"    0.5 (App (App (Macro "AND") (Macro "TRUE")) (Macro "FALSE"))
                                     (parseLambda "((AND TRUE) FALSE)")
    assertEq "abs over macro"    0.5 (Abs "x" (Macro "TRUE"))             (parseLambda "\\x.TRUE")
    assertEq "complex"           0.5 (App (Abs "x" (App (Macro "AND") vx)) (Macro "FALSE"))
                                     (parseLambda "((\\x.(AND x)) FALSE)")

parseLineTests :: TestCase ()
parseLineTests = group "3.3 parseLine" $ do
    assertEq  "Eval var"               0.5 (Right (Eval vx))                  (parseLine "x")
    assertEq  "Eval app (no parens)"   0.5 (Right (Eval (App vx vx)))         (parseLine "x x")
    assertEq  "Eval λx.(x x)"          0.5 (Right (Eval m))                   (parseLine "\\x.(x x)")
    assertEq  "Binding simple"         0.5 (Right (Binding "X" vx))           (parseLine "X=x")
    assertEq  "Binding K"              0.5 (Right (Binding "TRUE" k))         (parseLine "TRUE=\\x.\\y.x")
    assertErr "empty input is error"   0.5 (parseLine "")
