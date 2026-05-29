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
vars = undefined

-- 1.2.
freeVars :: Lambda -> [String]
freeVars = undefined

-- 1.3.
newVar :: [String] -> String
newVar = undefined

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm = undefined

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce = undefined

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep = undefined

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep = undefined

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify = undefined

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
