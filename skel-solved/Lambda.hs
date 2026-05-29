module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x)     = [x]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e)   = nub (x : vars e)
vars (Macro _)   = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x)     = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e)   = freeVars e \\ [x]
freeVars (Macro _)   = []

-- 1.3.
-- Smallest string (by length, then alphabetical) not in the list.
newVar :: [String] -> String
newVar xs = head [v | v <- candidates, v `notElem` xs]
  where
    candidates = concat [stringsOfLen n | n <- [1 ..]]
    stringsOfLen 1 = map (: []) ['a' .. 'z']
    stringsOfLen n = [c : rest | c <- ['a' .. 'z'], rest <- stringsOfLen (n - 1)]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _)             = True
isNormalForm (Macro _)           = True
isNormalForm (Abs _ e)           = isNormalForm e
isNormalForm (App (Abs _ _) _)   = False
isNormalForm (App e1 e2)         = isNormalForm e1 && isNormalForm e2

-- 1.5.
-- reduce x e1 e2: substitute x with e2 inside e1, renaming binders to avoid capture.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var v) e2
    | v == x    = e2
    | otherwise = Var v
reduce _ (Macro m) _ = Macro m
reduce x (App a b) e2 = App (reduce x a e2) (reduce x b e2)
reduce x abs1@(Abs v body) e2
    | v == x                  = abs1                    -- shadowed: no substitution
    | v `notElem` freeVars e2 = Abs v (reduce x body e2)
    | otherwise               =                          -- v would capture a free var of e2: rename
        let fresh = newVar (freeVars body ++ freeVars e2 ++ [x])
            body' = reduce v body (Var fresh)
        in Abs fresh (reduce x body' e2)

-- 1.6. Normal order: leftmost-outermost redex.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x body) e2) = reduce x body e2
normalStep (App e1 e2)
    | not (isNormalForm e1) = App (normalStep e1) e2
    | otherwise             = App e1 (normalStep e2)
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7. Applicative order: leftmost-innermost redex.
applicativeStep :: Lambda -> Lambda
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | not (isNormalForm e2) = App e1 (applicativeStep e2)
    | otherwise = case e1 of
        Abs x body -> reduce x body e2
        _          -> App e1 e2
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
