import System.IO
import Data.List (intercalate)

import Lambda
import Parser
import Code
import Default

main :: IO ()
main = interpreter defaultContext

interpreter :: Context -> IO ()
interpreter ctx = do
    putStr "λ> "
    hFlush stdout
    input <- getLine
    case input of
        ":q"   -> return ()
        ":r"   -> interpreter defaultContext
        ":ctx" -> printList ctx >> interpreter ctx
        _ -> case parseLine input of
            Left err              -> putStrLn ("Error: " ++ err) >> interpreter ctx
            Right (Binding s l)   -> interpreter ((s, l) : ctx)
            Right (Eval l)        -> case normalCtx ctx l of
                Left err -> putStrLn ("Error: undefined macro " ++ err) >> interpreter ctx
                Right ls -> printList ls >> interpreter ctx

printList :: Show a => [a] -> IO ()
printList = putStrLn . intercalate "\n" . map show
