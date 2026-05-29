module Tests.Grader where

import Control.Exception (SomeException, try, evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import System.IO (hFlush, stdout)

runTests :: TestCase () -> IO ()
assert :: String -> Pts -> Bool -> TestCase ()
assertEq :: (Eq a, Show a) => String -> Pts -> a -> a -> TestCase ()
assertNeq :: (Eq a, Show a) => String -> Pts -> a -> a -> TestCase ()
assertErr :: String -> Pts -> Either String a -> TestCase () -- NOT runtime error
group :: String -> TestCase () -> TestCase ()

type Pts = Double
newtype Points = Points (Pts, Pts) deriving (Show)

showPts :: Pts -> String
showPts x
    | x == fromIntegral (truncate x :: Integer) = show (truncate x :: Integer)
    | otherwise                                  = show x

instance Semigroup Points where
    (Points (x1, y1)) <> (Points (x2, y2)) = Points (x1 + x2, y1 + y2)

instance Monoid Points where
    mempty = Points (0, 0)

type TestCase = ReaderT Int (WriterT Points IO)

runTests test_suite = do
    (_, Points (earned, total)) <- runWriterT (runReaderT test_suite 0)
    putStrLn $ "Total: " ++ showPts earned ++ "/" ++ showPts total

assert name pts condition = do
    depth <- ask
    rt <- liftIO (try (evaluate condition) :: IO (Either SomeException Bool))
    let (passed, msgs) = case rt of
                            Left err -> (False, [red $ "Error: " ++ show err])
                            Right val -> (val, [])
    liftIO $ printOutput depth pts passed name msgs
    tell $ Points (if passed then pts else 0, pts)

assertEq name pts expected actual = do
    depth <- ask
    rt <- liftIO (try (evaluate (expected == actual)) :: IO (Either SomeException Bool))
    let (passed, msgs) = case rt of
                            Left err -> (False, [red $ "Error: " ++ show err])
                            Right val -> if val
                                then (True, [])
                                else (False, ["Expected: " ++ show expected, "Actual: " ++ show actual])
    liftIO $ printOutput depth pts passed name msgs
    tell $ Points (if passed then pts else 0, pts)

assertNeq name pts expected actual = assert name pts (expected /= actual)

assertErr name pts val = do
    depth <- ask
    rt <- liftIO $ (try (evaluate (case val of { Left _ -> True; Right _ -> False }))
                    :: IO (Either SomeException Bool))
    let (passed, msgs) = case rt of
            Left err     -> (False, [red $ "Error: " ++ show err])
            Right True   -> (True, [])
            Right False  -> (False, ["Expected Left (error), got Right"])
    liftIO $ printOutput depth pts passed name msgs
    tell $ Points (if passed then pts else 0, pts)

group heading inner = do
  depth <- ask
  let indent = replicate (max 0 (10 - 2 * depth)) '='
  liftIO $ putStrLn $ pad depth ++ indent ++ " " ++ heading ++ " " ++ indent
  local (+1) inner

red :: String -> String
-- red msg = msg -- uncomment this line to disable color
red msg = "\x1b[31m" ++ msg ++ "\x1b[0m"

green :: String -> String
-- green msg = msg -- uncomment this line to disable color
green msg = "\x1b[32m" ++ msg ++ "\x1b[0m"

pad :: Int -> String
pad depth = replicate (depth * 2) ' '

printOutput :: Int -> Pts -> Bool -> String -> [String] -> IO ()
printOutput depth pts passed name msgs = do
    let status = if passed then green "[PASSED]" else red "[FAILED]"
    putStrLn $ pad depth ++ status ++ " " ++ name ++ " (" ++ showPts pts ++ " pts) "
    mapM_ (putStrLn . (pad (depth + 1) ++)) msgs
    hFlush stdout
