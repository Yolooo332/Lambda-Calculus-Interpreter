module DeBruijn where

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

-- 4.1.
toDB :: Context -> Lambda -> DeBruijn
toDB = undefined

-- 4.2.
fromDB :: Context -> DeBruijn -> Lambda
fromDB = undefined

-- 4.3.
isNormalForm :: DeBruijn -> Bool
isNormalForm = undefined

-- 4.4.
reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce = undefined

-- 4.5.
normalStep :: DeBruijn -> DeBruijn
normalStep = undefined

-- 4.6.
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep = undefined

-- 4.7.
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify = undefined

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep
