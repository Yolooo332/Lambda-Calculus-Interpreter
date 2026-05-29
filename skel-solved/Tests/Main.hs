module Main where

import System.Environment

import Tests.Grader
import Tests.LambdaTest   (lambdaTests)
import Tests.ParserTest   (parserTests)
import Tests.CodeTest     (codeTests)
import Tests.DeBruijnTest (deBruijnTests)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["lambda"]   -> runTests $ group "Lambda"           lambdaTests
        ["parser"]   -> runTests $ group "Parser"           parserTests
        ["code"]     -> runTests $ group "Code"             codeTests
        ["debruijn"] -> runTests $ group "DeBruijn (extra)" deBruijnTests
        []           -> runTests allTests
        _            -> putStrLn "Usage: run_tests [lambda|parser|code|debruijn]"

allTests :: TestCase ()
allTests = do
    group "Lambda"           lambdaTests
    group "Parser"           parserTests
    group "Code"             codeTests
    group "DeBruijn (extra)" deBruijnTests
