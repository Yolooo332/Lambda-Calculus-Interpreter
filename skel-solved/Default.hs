module Default where

import Lambda
import Code

vx, vy, vz, vf, vm, vn :: Lambda
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vm = Var "m"
vn = Var "n"

m, i, k, ki, c, y :: Lambda
m  = Abs "x" $ App vx vx
i  = Abs "x" $ vx
k  = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c  = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y  = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

defaultContext :: Context
defaultContext =
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    ]
