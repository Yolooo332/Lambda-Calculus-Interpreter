module DeBruijn where

import Data.List (elemIndex)

import Lambda (Lambda(..))

type Context = [String]

data DeBruijn = DBVar Int
              | DBFree String
              | DBApp DeBruijn DeBruijn
              | DBAbs String DeBruijn

instance Show DeBruijn where
    show (DBVar n) = show n
    show (DBFree x) = x
    show (DBApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (DBAbs _ e) = "λ " ++ show e

instance Eq DeBruijn where
    (DBVar n) == (DBVar m) = n == m
    (DBFree _) == (DBFree _) = True
    (DBApp e1 e2) == (DBApp f1 f2) = e1 == f1 && e2 == f2
    (DBAbs _ e) == (DBAbs _ f) = e == f
    _ == _ = False

-- 4.1. Context is the list of binders, innermost first.
toDB :: Context -> Lambda -> DeBruijn
toDB ctx (Var x)     = case elemIndex x ctx of
    Just i  -> DBVar i
    Nothing -> DBFree x
toDB ctx (App e1 e2) = DBApp (toDB ctx e1) (toDB ctx e2)
toDB ctx (Abs x e)   = DBAbs x (toDB (x : ctx) e)
toDB _   (Macro m)   = DBFree m

-- 4.2.
fromDB :: Context -> DeBruijn -> Lambda
fromDB _   (DBFree x)    = Var x
fromDB ctx (DBVar i)     = Var (ctx !! i)
fromDB ctx (DBApp e1 e2) = App (fromDB ctx e1) (fromDB ctx e2)
fromDB ctx (DBAbs x e)   = Abs x (fromDB (x : ctx) e)

-- 4.3.
isNormalForm :: DeBruijn -> Bool
isNormalForm (DBVar _)             = True
isNormalForm (DBFree _)            = True
isNormalForm (DBAbs _ e)           = isNormalForm e
isNormalForm (DBApp (DBAbs _ _) _) = False
isNormalForm (DBApp e1 e2)         = isNormalForm e1 && isNormalForm e2

-- Shift free indices (i.e. those >= cutoff) by d.
shift :: Int -> Int -> DeBruijn -> DeBruijn
shift d c (DBVar i)
    | i >= c    = DBVar (i + d)
    | otherwise = DBVar i
shift _ _ (DBFree x)    = DBFree x
shift d c (DBApp e1 e2) = DBApp (shift d c e1) (shift d c e2)
shift d c (DBAbs x e)   = DBAbs x (shift d (c + 1) e)

-- Substitute the binder at depth j with v (with v's free indices shifted).
subst :: Int -> DeBruijn -> DeBruijn -> DeBruijn
subst j v (DBVar i)
    | i == j    = v
    | i >  j    = DBVar (i - 1)   -- one binder vanished above us
    | otherwise = DBVar i
subst _ _ (DBFree x)    = DBFree x
subst j v (DBApp e1 e2) = DBApp (subst j v e1) (subst j v e2)
subst j v (DBAbs x e)   = DBAbs x (subst (j + 1) (shift 1 0 v) e)

-- 4.4. reduce val e = body where index 0 is replaced by val.
reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce val body = subst 0 val body

-- 4.5.
normalStep :: DeBruijn -> DeBruijn
normalStep (DBApp (DBAbs _ body) v) = reduce v body
normalStep (DBApp e1 e2)
    | not (isNormalForm e1) = DBApp (normalStep e1) e2
    | otherwise             = DBApp e1 (normalStep e2)
normalStep (DBAbs x e) = DBAbs x (normalStep e)
normalStep e = e

-- 4.6.
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep (DBAbs x e) = DBAbs x (applicativeStep e)
applicativeStep (DBApp e1 e2)
    | not (isNormalForm e1) = DBApp (applicativeStep e1) e2
    | not (isNormalForm e2) = DBApp e1 (applicativeStep e2)
    | otherwise = case e1 of
        DBAbs _ body -> reduce e2 body
        _            -> DBApp e1 e2
applicativeStep e = e

-- 4.7.
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep
