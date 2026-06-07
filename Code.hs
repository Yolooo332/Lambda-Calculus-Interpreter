module Code where

import Lambda

type Context = [(String, Lambda)]

-- expand _ (Var x) = Right (Var x) first
-- return simplify step e'

data Line
  = Eval Lambda
  | Binding String Lambda
  deriving (Eq)

instance Show Line where
  show (Eval l) = show l
  show (Binding s l) = s ++ " = " ++ show l

expand :: Context -> Lambda -> Either String Lambda
expand _ (Var x) = Right (Var x)
expand ctx (App e1 e2) = do
  e1' <- expand ctx e1
  e2' <- expand ctx e2
  return (App e1' e2')
expand ctx (Abs x e) = do
  e' <- expand ctx e
  return (Abs x e')
expand ctx (Macro m) = case lookup m ctx of
  Just l -> expand ctx l
  Nothing -> Left m

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step e = do
  e' <- expand ctx e
  return (simplify step e')

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
