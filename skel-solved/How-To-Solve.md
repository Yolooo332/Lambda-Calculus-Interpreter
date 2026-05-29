# How To Solve — Lambda Calculus Interpreter in Haskell

A step-by-step walkthrough of every function in the order it appears in the
assignment. For each task you get the code, the reasoning, and the gotchas to
watch out for. Aimed at someone who knows Scala and can read Haskell.

The full TDA we are working with:

```haskell
data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String
```

Four constructors, four pattern-match branches in almost every function. If you
keep that mental model you will never miss a case.

---

## Part 1 — Evaluation (`Lambda.hs`)

### 1.1 `vars` — list every variable name in an expression

We want the set of *all* names that appear, including binder names. The list
should be deduplicated (the test sorts but expects no duplicates).

```haskell
vars :: Lambda -> [String]
vars (Var x)     = [x]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e)   = nub (x : vars e)
vars (Macro _)   = []
```

- `Var x`: single variable, return `[x]`.
- `App`: union of vars from both sides; `nub` removes duplicates.
- `Abs x e`: include the binder `x` and merge with the body's vars.
- `Macro`: not a variable, so empty list.

`nub` is from `Data.List`; it keeps the first occurrence of each element.

### 1.2 `freeVars` — list only the free variables

A variable is free if it appears outside the scope of any `Abs` that binds it.
Same shape as `vars` but for `Abs x e` we *remove* `x`.

```haskell
freeVars :: Lambda -> [String]
freeVars (Var x)     = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e)   = freeVars e \\ [x]
freeVars (Macro _)   = []
```

The operator `\\` (from `Data.List`) is list difference. Since the recursive
calls already produce deduplicated lists (thanks to `nub`), removing `[x]` once
is enough.

### 1.3 `newVar` — smallest fresh name

The expected order from the tests is **length first, then alphabetical**:
`a, b, ..., z, aa, ab, ..., az, ba, ..., zz, aaa, ...`

Generate that infinite stream and pick the first name not in the input list:

```haskell
newVar :: [String] -> String
newVar xs = head [v | v <- candidates, v `notElem` xs]
  where
    candidates = concat [stringsOfLen n | n <- [1 ..]]
    stringsOfLen 1 = map (: []) ['a' .. 'z']
    stringsOfLen n = [c : rest | c <- ['a' .. 'z'], rest <- stringsOfLen (n - 1)]
```

Key detail: in `stringsOfLen n` the *outer* generator is `c <- ['a'..'z']`.
This makes the **leading char vary slowest**, which is alphabetical order:
`aa, ab, ac, ..., az, ba, ...`. If you flip the generators you get the wrong
order (`aa, ba, ca, ...`).

Haskell's laziness means the infinite stream is fine: `head [...]` stops as
soon as it finds the first match.

### 1.4 `isNormalForm` — no redex anywhere

A redex is `App (Abs _ _) _`. Walk the tree; if you find one, return `False`.

```haskell
isNormalForm :: Lambda -> Bool
isNormalForm (Var _)             = True
isNormalForm (Macro _)           = True
isNormalForm (Abs _ e)           = isNormalForm e
isNormalForm (App (Abs _ _) _)   = False
isNormalForm (App e1 e2)         = isNormalForm e1 && isNormalForm e2
```

The order of the two `App` branches matters: the specific `App (Abs _ _) _`
case must come first so it fires before the generic `App e1 e2` case.

### 1.5 `reduce` — capture-avoiding substitution

Signature: `reduce x e1 e2` means "substitute every free `x` in `e1` with `e2`".

Three things to handle for `Abs v body`:
1. If the binder shadows `x` (`v == x`), stop — `x` is no longer free here.
2. If `v` is free in `e2`, substituting naively would *capture* it. Rename
   the binder to a fresh name first, then continue.
3. Otherwise just recurse into the body.

```haskell
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var v) e2
    | v == x    = e2
    | otherwise = Var v
reduce _ (Macro m) _  = Macro m
reduce x (App a b) e2 = App (reduce x a e2) (reduce x b e2)
reduce x abs1@(Abs v body) e2
    | v == x                  = abs1                       -- shadowed
    | v `notElem` freeVars e2 = Abs v (reduce x body e2)
    | otherwise               =                            -- avoid capture
        let fresh = newVar (freeVars body ++ freeVars e2 ++ [x])
            body' = reduce v body (Var fresh)
        in Abs fresh (reduce x body' e2)
```

The rename step uses `reduce` itself: we replace `v` inside `body` with a fresh
variable, then carry on with the original substitution of `x`. The fresh name
must avoid every name currently visible — body vars, `e2`'s free vars, and `x`
itself.

Concrete example from the tests:

```
reduce "x" (λy.(x y)) (λx.y)   =   λa.(λx.y a)
```

`y` is free in `λx.y`, so the binder `y` in `λy.(x y)` would capture it. We
rename to `a`, getting `λa.(x a)`, then substitute `x` with `λx.y`, yielding
`λa.(λx.y a)`.

### 1.6 `normalStep` — one step of normal-order reduction

Normal order = **leftmost outermost**. If the head of an `App` is an `Abs`, that
is the redex and we fire it. Otherwise descend into the head first, and only
into the argument if the head is already a normal form.

```haskell
normalStep :: Lambda -> Lambda
normalStep (App (Abs x body) e2) = reduce x body e2
normalStep (App e1 e2)
    | not (isNormalForm e1) = App (normalStep e1) e2
    | otherwise             = App e1 (normalStep e2)
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e
```

The catch-all `normalStep e = e` covers `Var` and `Macro`: they are already in
NF, so a step is a no-op.

### 1.7 `applicativeStep` — one step of applicative-order reduction

Applicative order = **leftmost innermost**. Same idea, but before firing the
top-level redex we must first reduce the argument until it is in NF.

```haskell
applicativeStep :: Lambda -> Lambda
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | not (isNormalForm e2) = App e1 (applicativeStep e2)
    | otherwise = case e1 of
        Abs x body -> reduce x body e2
        _          -> App e1 e2
applicativeStep e = e
```

The order of guards encodes the strategy:
1. Reduce the head if it is not NF.
2. Then reduce the argument if it is not NF.
3. Only when both sides are NF, fire the redex (if the head is an `Abs`).

This is why `applicativeStep` "gets stuck" on `(λx.y) Ω`: it keeps trying to
reduce `Ω = (λx.x x)(λx.x x)`, which steps to itself, never letting the outer
redex fire.

### 1.8 `simplify` — apply steps until normal form

Just iterate the step function and collect each intermediate value. Stop when
we hit NF.

```haskell
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal      = simplify normalStep
applicative = simplify applicativeStep
```

The first element of the returned list is the input; the last is the normal
form. If the expression has no NF (e.g. `Ω`), the list is infinite — the
caller decides how many steps to `take`.

---

## Part 2 — Parsing (`Parser.hs`)

The grammar:

```
<lambda>   ::= <abs> | <app>
<abs>      ::= '\' <variable> '.' <lambda>
<app>      ::= <atom> (' ' <atom>)*
<atom>     ::= '(' <lambda> ')' | <variable> | <macro>
<variable> ::= [a-z]+
<macro>    ::= [A-Z0-9]+        (added for task 3)
```

We must use the given `newtype`:

```haskell
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
```

A parser is a function from input to "either I failed (Nothing) or here is the
result and the remaining input (Just (a, rest))".

### Instances — make it a monad with choice

This is boilerplate but crucial. Once you have `Functor`, `Applicative`,
`Monad`, and `Alternative`, you can write parsers in `do`-notation and combine
them with `<|>`.

```haskell
instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> case p s of
        Nothing      -> Nothing
        Just (a, rs) -> Just (f a, rs)

instance Applicative Parser where
    pure a = Parser $ \s -> Just (a, s)
    Parser pf <*> Parser pa = Parser $ \s -> case pf s of
        Nothing      -> Nothing
        Just (f, rs) -> case pa rs of
            Nothing       -> Nothing
            Just (a, rs') -> Just (f a, rs')

instance Monad Parser where
    return = pure
    Parser p >>= f = Parser $ \s -> case p s of
        Nothing      -> Nothing
        Just (a, rs) -> parse (f a) rs

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
        Nothing -> p2 s
        r       -> r

instance MonadPlus Parser
```

The critical thing about `<|>`: when `p1` fails, `p2` is run on the **original**
string `s`, never on whatever `p1` was looking at when it gave up. This is
"automatic backtracking" and is what lets us write `bindingP <|> evalP` without
worrying about partial consumption.

### Primitive combinators

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    (c:rs) | f c -> Just (c, rs)
    _            -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

variableP :: Parser String
variableP = some (satisfy isLower)         -- one or more lowercase

macroP :: Parser String
macroP = some (satisfy (\c -> isUpper c || isDigit c))   -- one or more upper/digit
```

`some p` (from `Alternative`) is "one or more of `p`"; `many p` is "zero or
more". We get them for free from the `Alternative` instance.

### Grammar combinators

```haskell
atomP :: Parser Lambda
atomP =  parens lambdaP
     <|> (Var   <$> variableP)
     <|> (Macro <$> macroP)
  where
    parens p = charP '(' *> p <* charP ')'

appP :: Parser Lambda
appP = do
    first <- atomP
    rest  <- many (charP ' ' *> atomP)
    return (foldl App first rest)

absP :: Parser Lambda
absP = do
    _ <- charP '\\'
    v <- variableP
    _ <- charP '.'
    body <- lambdaP
    return (Abs v body)

lambdaP :: Parser Lambda
lambdaP = absP <|> appP
```

Two notes:

1. `appP` is left-associative because `foldl App first rest` builds
   `App (App (App first a1) a2) a3 ...`. So `x y z` parses as `((x y) z)`.
2. `absP` consumes the entire body via `lambdaP`. That is why `\x.x y` parses
   as `\x.(x y)` — the body greedily grabs everything up to the next `)` or
   end of input.

### 2.1 / 3.2 `parseLambda`

Top-level entry. Runs `lambdaP` on the input and returns the lambda. The macro
case is already included in `atomP`, so this same function works for task 3.2.

```haskell
parseLambda :: String -> Lambda
parseLambda s = case parse lambdaP s of
    Just (l, _) -> l
    Nothing     -> error ("parseLambda: cannot parse " ++ s)
```

The tests only feed valid input, so an `error` on failure is fine here. We
return `l` even if there is trailing input — `parseLine` is stricter about
that.

---

## Part 3 — Macros and code (`Code.hs` + `Parser.hs`)

### 3.1 `simplifyCtx` — evaluate an expression that may contain macros

Two phases:
1. **Expand** every `Macro` to the expression it stands for, looking it up in
   the context. If a macro is missing, return `Left m`.
2. **Simplify** the macro-free expression using `Lambda.simplify`.

```haskell
expand :: Context -> Lambda -> Either String Lambda
expand _   (Var x)     = Right (Var x)
expand ctx (App e1 e2) = do
    e1' <- expand ctx e1
    e2' <- expand ctx e2
    return (App e1' e2')
expand ctx (Abs x e)   = do
    e' <- expand ctx e
    return (Abs x e')
expand ctx (Macro m)   = case lookup m ctx of
    Just l  -> expand ctx l    -- recursive: definitions may contain macros
    Nothing -> Left m

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step e = do
    e' <- expand ctx e
    return (simplify step e')
```

Notice how clean the `Either` monad makes this: each `<-` short-circuits to
`Left` on the first missing macro. No nested `case` statements needed.

The hint in the assignment ("can we reuse `simplify`?") points exactly here:
once macros are gone, the existing logic handles everything.

`normalCtx` and `applicativeCtx` are one-liners that fix the step strategy.

### 3.2 Parsing macros

Already done in `atomP` above (the `Macro <$> macroP` branch). The parser
accepts any non-empty sequence of uppercase letters or digits as a macro.

### 3.3 `parseLine` — a line is either an evaluation or a binding

```haskell
data Line = Eval Lambda
          | Binding String Lambda
```

A binding has the form `MACRONAME=<lambda>`. An eval is just a lambda.

```haskell
bindingP :: Parser Line
bindingP = do
    name <- macroP
    _    <- charP '='
    body <- lambdaP
    return (Binding name body)

lineP :: Parser Line
lineP = bindingP <|> (Eval <$> lambdaP)

parseLine :: String -> Either String Line
parseLine s = case parse lineP s of
    Just (l, "") -> Right l
    Just (_, rs) -> Left ("unexpected trailing input: " ++ rs)
    Nothing      -> Left "parse error"
```

Try `bindingP` first. If the input starts with a macro name but the next char
is not `=`, `bindingP` fails and `<|>` backtracks to the original string, then
`Eval <$> lambdaP` tries again from scratch. That is how `X` alone parses as
`Eval (Macro "X")` while `X=y` parses as `Binding "X" (Var "y")`.

Unlike `parseLambda`, here we require the *whole* input to be consumed — a
trailing remainder means the user wrote garbage.

---

## Part 4 — De Bruijn (`DeBruijn.hs`)

The point of De Bruijn indices: bound variables become numbers ("how many
binders up"), so alpha-equivalence becomes structural equality and there is no
more variable capture to worry about. The price is that you have to fiddle
with indices when you push values across binders.

```haskell
data DeBruijn = DBVar Int
              | DBFree String
              | DBApp DeBruijn DeBruijn
              | DBAbs String DeBruijn   -- name kept only as a hint
```

### 4.1 `toDB` — convert names to indices

We carry a `Context = [String]`: the list of binders currently in scope,
**innermost first**. When we see a `Var x`, look it up by position; if missing,
it is free.

```haskell
toDB :: Context -> Lambda -> DeBruijn
toDB ctx (Var x)     = case elemIndex x ctx of
    Just i  -> DBVar i
    Nothing -> DBFree x
toDB ctx (App e1 e2) = DBApp (toDB ctx e1) (toDB ctx e2)
toDB ctx (Abs x e)   = DBAbs x (toDB (x : cx) e)
  where cx = ctx
toDB _   (Macro m)   = DBFree m
```

(In the actual file the `where` is inlined: `toDB (x : ctx) e`.)

When we enter `Abs x`, we prepend `x` to the context so it becomes index `0`
for the body. Macros are treated as free variables, per the spec.

### 4.2 `fromDB` — convert indices back to names

The inverse direction. The context tells us which name corresponds to each
index. We use the `DBAbs` name hint to reconstruct binder names.

```haskell
fromDB :: Context -> DeBruijn -> Lambda
fromDB _   (DBFree x)    = Var x
fromDB ctx (DBVar i)     = Var (ctx !! i)
fromDB ctx (DBApp e1 e2) = App (fromDB ctx e1) (fromDB ctx e2)
fromDB ctx (DBAbs x e)   = Abs x (fromDB (x : ctx) e)
```

`toDB [] . fromDB []` is the identity on well-formed terms.

### 4.3 `isNormalForm` — same shape as the named version

```haskell
isNormalForm :: DeBruijn -> Bool
isNormalForm (DBVar _)             = True
isNormalForm (DBFree _)            = True
isNormalForm (DBAbs _ e)           = isNormalForm e
isNormalForm (DBApp (DBAbs _ _) _) = False
isNormalForm (DBApp e1 e2)         = isNormalForm e1 && isNormalForm e2
```

### 4.4 `reduce` — the De Bruijn beta-reduction

This is where index arithmetic shows up. Two helpers:

- **`shift d c e`**: add `d` to every index in `e` that is `>= c`. Free
  indices (those not bound inside `e`) get shifted; bound ones don't.
- **`subst j v e`**: replace `DBVar j` with `v` everywhere in `e`. Indices
  greater than `j` get decremented by 1 (because one binder has been
  "consumed"). When we cross into a `DBAbs`, both `j` and the cutoff for
  shifting `v` go up by one.

```haskell
shift :: Int -> Int -> DeBruijn -> DeBruijn
shift d c (DBVar i)
    | i >= c    = DBVar (i + d)
    | otherwise = DBVar i
shift _ _ (DBFree x)    = DBFree x
shift d c (DBApp e1 e2) = DBApp (shift d c e1) (shift d c e2)
shift d c (DBAbs x e)   = DBAbs x (shift d (c + 1) e)

subst :: Int -> DeBruijn -> DeBruijn -> DeBruijn
subst j v (DBVar i)
    | i == j    = v
    | i >  j    = DBVar (i - 1)
    | otherwise = DBVar i
subst _ _ (DBFree x)    = DBFree x
subst j v (DBApp e1 e2) = DBApp (subst j v e1) (subst j v e2)
subst j v (DBAbs x e)   = DBAbs x (subst (j + 1) (shift 1 0 v) e)

reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce val body = subst 0 val body
```

Quick sanity check:

- `reduce a (DBVar 0)` = `a` — the bound variable becomes the value. Good.
- `reduce a (DBVar 1)` = `DBVar 0` — that index pointed *past* the outer
  lambda; with the lambda gone, it should point one step less far.
- `reduce a (DBAbs y (DBVar 1))` = `DBAbs y a` — index 1 inside the inner
  abstraction means "one binder up", which from the inside is *past* the inner
  binder and matches the outer one. After substituting and shifting `a` (no
  free indices, so shift is a no-op), we get `DBAbs y a`.

No name freshness, no capture detection — just bookkeeping.

### 4.5 / 4.6 `normalStep` and `applicativeStep`

Mechanically the same as the named versions, just using `DBApp`/`DBAbs`.

```haskell
normalStep :: DeBruijn -> DeBruijn
normalStep (DBApp (DBAbs _ body) v) = reduce v body
normalStep (DBApp e1 e2)
    | not (isNormalForm e1) = DBApp (normalStep e1) e2
    | otherwise             = DBApp e1 (normalStep e2)
normalStep (DBAbs x e) = DBAbs x (normalStep e)
normalStep e = e

applicativeStep :: DeBruijn -> DeBruijn
applicativeStep (DBAbs x e) = DBAbs x (applicativeStep e)
applicativeStep (DBApp e1 e2)
    | not (isNormalForm e1) = DBApp (applicativeStep e1) e2
    | not (isNormalForm e2) = DBApp e1 (applicativeStep e2)
    | otherwise = case e1 of
        DBAbs _ body -> reduce e2 body
        _            -> DBApp e1 e2
applicativeStep e = e
```

This is the payoff: the strategies are basically identical to the named code,
but `reduce` is now trivial.

### 4.7 `simplify` — iterate to NF

```haskell
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal      = simplify normalStep
applicative = simplify applicativeStep
```

Literally a copy of the named version with the type swapped.

---

## Putting it together

When you run the REPL (`make repl && ./repl`):

1. The user types a line.
2. `parseLine` produces either a `Binding` (added to the context) or an
   `Eval l` (a lambda to reduce).
3. For `Eval`, `normalCtx ctx l` expands macros via `expand`, then `simplify
   normalStep` produces the reduction trace.
4. The trace is printed; the prompt comes back.

Every step uses code we wrote. The four parts compose cleanly because each
layer (substitution, parsing, macros, De Bruijn) was kept small and orthogonal.

## Things that tripped me up — so you don't trip

- **`newVar` ordering.** The naive list comprehension generates names in the
  wrong order. Put the leading character in the outer generator.
- **Capture avoidance in `reduce`.** It is easy to forget the `v ==  x`
  shadowing case, or to compute the "names to avoid" incompletely. Include
  `freeVars body`, `freeVars e2`, and `[x]`.
- **`<|>` semantics.** It backtracks to the original string. That is why
  `bindingP <|> evalP` works even though `bindingP` may consume the macro name
  before failing on `=`.
- **`appP` is left-associative.** Use `foldl App`, not `foldr`.
- **De Bruijn `shift` cutoff.** When `subst` enters an abstraction, both the
  substitution depth and the shift cutoff increase by one.
- **Pattern order in `isNormalForm`.** The redex case `App (Abs _ _) _` must
  come *before* the generic `App` case.
