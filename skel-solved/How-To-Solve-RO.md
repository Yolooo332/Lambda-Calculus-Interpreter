# Cum Se Rezolvă — Interpretor de Lambda Calcul în Haskell

O prezentare pas cu pas a fiecărei funcții, în ordinea în care apare în
temă. Pentru fiecare sarcină primești codul, raționamentul și capcanele de
care trebuie să te ferești. Adresat cuiva care știe Scala și poate citi Haskell.

TDA-ul complet cu care lucrăm:

```haskell
data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String
```

Patru constructori, patru ramuri de pattern-matching în aproape fiecare funcție.
Dacă păstrezi acest model mental, nu vei rata niciodată vreun caz.

---

## Partea 1 — Evaluare (`Lambda.hs`)

### 1.1 `vars` — listează fiecare nume de variabilă dintr-o expresie

Vrem mulțimea *tuturor* numelor care apar, inclusiv numele de legători (binders).
Lista trebuie să fie fără duplicate (testul sortează, dar nu se așteaptă la
duplicate).

```haskell
vars :: Lambda -> [String]
vars (Var x)     = [x]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e)   = nub (x : vars e)
vars (Macro _)   = []
```

- `Var x`: o singură variabilă, întoarce `[x]`.
- `App`: reuniunea variabilelor din ambele părți; `nub` elimină duplicatele.
- `Abs x e`: include legătorul `x` și combină cu variabilele din corp.
- `Macro`: nu e o variabilă, deci listă vidă.

`nub` provine din `Data.List`; păstrează prima apariție a fiecărui element.

### 1.2 `freeVars` — listează doar variabilele libere

O variabilă este liberă dacă apare în afara domeniului oricărui `Abs` care o
leagă. Aceeași formă ca `vars`, dar pentru `Abs x e` îl *eliminăm* pe `x`.

```haskell
freeVars :: Lambda -> [String]
freeVars (Var x)     = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e)   = freeVars e \\ [x]
freeVars (Macro _)   = []
```

Operatorul `\\` (din `Data.List`) este diferența de liste. Deoarece apelurile
recursive produc deja liste fără duplicate (mulțumită lui `nub`), eliminarea lui
`[x]` o singură dată este suficientă.

### 1.3 `newVar` — cel mai mic nume nefolosit

Ordinea așteptată de teste este **întâi după lungime, apoi alfabetic**:
`a, b, ..., z, aa, ab, ..., az, ba, ..., zz, aaa, ...`

Generează acel flux infinit și alege primul nume care nu se află în lista de
intrare:

```haskell
newVar :: [String] -> String
newVar xs = head [v | v <- candidates, v `notElem` xs]
  where
    candidates = concat [stringsOfLen n | n <- [1 ..]]
    stringsOfLen 1 = map (: []) ['a' .. 'z']
    stringsOfLen n = [c : rest | c <- ['a' .. 'z'], rest <- stringsOfLen (n - 1)]
```

Detaliu cheie: în `stringsOfLen n`, generatorul *exterior* este `c <- ['a'..'z']`.
Astfel **caracterul de la început variază cel mai lent**, ceea ce dă ordinea
alfabetică: `aa, ab, ac, ..., az, ba, ...`. Dacă inversezi generatoarele, obții
ordinea greșită (`aa, ba, ca, ...`).

Evaluarea leneșă (lazy) din Haskell face ca fluxul infinit să nu fie o problemă:
`head [...]` se oprește imediat ce găsește prima potrivire.

### 1.4 `isNormalForm` — niciun redex nicăieri

Un redex este `App (Abs _ _) _`. Parcurge arborele; dacă găsești unul, întoarce
`False`.

```haskell
isNormalForm :: Lambda -> Bool
isNormalForm (Var _)             = True
isNormalForm (Macro _)           = True
isNormalForm (Abs _ e)           = isNormalForm e
isNormalForm (App (Abs _ _) _)   = False
isNormalForm (App e1 e2)         = isNormalForm e1 && isNormalForm e2
```

Ordinea celor două ramuri pentru `App` contează: cazul specific
`App (Abs _ _) _` trebuie să fie primul, ca să se declanșeze înaintea cazului
generic `App e1 e2`.

### 1.5 `reduce` — substituție care evită capturarea

Semnătură: `reduce x e1 e2` înseamnă „substituie fiecare `x` liber din `e1` cu
`e2`".

Trei lucruri de tratat pentru `Abs v body`:
1. Dacă legătorul îl umbrește pe `x` (`v == x`), oprește-te — `x` nu mai este
   liber aici.
2. Dacă `v` este liber în `e2`, o substituție naivă l-ar *captura*. Redenumește
   mai întâi legătorul cu un nume nefolosit, apoi continuă.
3. Altfel, doar recursivează în corp.

```haskell
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var v) e2
    | v == x    = e2
    | otherwise = Var v
reduce _ (Macro m) _  = Macro m
reduce x (App a b) e2 = App (reduce x a e2) (reduce x b e2)
reduce x abs1@(Abs v body) e2
    | v == x                  = abs1                       -- umbrit
    | v `notElem` freeVars e2 = Abs v (reduce x body e2)
    | otherwise               =                            -- evită capturarea
        let fresh = newVar (freeVars body ++ freeVars e2 ++ [x])
            body' = reduce v body (Var fresh)
        in Abs fresh (reduce x body' e2)
```

Pasul de redenumire folosește chiar `reduce`: îl înlocuim pe `v` din interiorul
lui `body` cu o variabilă nefolosită, apoi continuăm cu substituția originală a
lui `x`. Numele nefolosit trebuie să evite fiecare nume vizibil în acel moment —
variabilele din corp, variabilele libere ale lui `e2` și pe `x` însuși.

Exemplu concret din teste:

```
reduce "x" (λy.(x y)) (λx.y)   =   λa.(λx.y a)
```

`y` este liber în `λx.y`, deci legătorul `y` din `λy.(x y)` l-ar captura.
Îl redenumim în `a`, obținând `λa.(x a)`, apoi îl substituim pe `x` cu `λx.y`,
rezultând `λa.(λx.y a)`.

### 1.6 `normalStep` — un pas de reducere în ordine normală

Ordinea normală = **cel mai din stânga, cel mai din exterior** (leftmost
outermost). Dacă capul unui `App` este un `Abs`, acela este redex-ul și îl
declanșăm. Altfel coborâm întâi în cap și abia apoi în argument, dacă capul este
deja în formă normală.

```haskell
normalStep :: Lambda -> Lambda
normalStep (App (Abs x body) e2) = reduce x body e2
normalStep (App e1 e2)
    | not (isNormalForm e1) = App (normalStep e1) e2
    | otherwise             = App e1 (normalStep e2)
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e
```

Cazul atrapă-tot `normalStep e = e` acoperă `Var` și `Macro`: ele sunt deja în
FN (formă normală), deci un pas nu schimbă nimic.

### 1.7 `applicativeStep` — un pas de reducere în ordine aplicativă

Ordinea aplicativă = **cel mai din stânga, cel mai din interior** (leftmost
innermost). Aceeași idee, dar înainte de a declanșa redex-ul de la nivelul de
sus trebuie mai întâi să reducem argumentul până când ajunge în FN.

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

Ordinea gărzilor codifică strategia:
1. Reduce capul dacă nu este în FN.
2. Apoi reduce argumentul dacă nu este în FN.
3. Doar când ambele părți sunt în FN, declanșează redex-ul (dacă capul este un
   `Abs`).

De aceea `applicativeStep` „se blochează" pe `(λx.y) Ω`: încearcă mereu să
reducă `Ω = (λx.x x)(λx.x x)`, care se reduce la el însuși, fără să lase vreodată
redex-ul exterior să se declanșeze.

### 1.8 `simplify` — aplică pași până la forma normală

Pur și simplu iterezi funcția de pas și colectezi fiecare valoare intermediară.
Te oprești când ajungi în FN.

```haskell
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal      = simplify normalStep
applicative = simplify applicativeStep
```

Primul element din lista întoarsă este intrarea; ultimul este forma normală.
Dacă expresia nu are FN (de ex. `Ω`), lista este infinită — apelantul decide
câți pași să ia cu `take`.

---

## Partea 2 — Parsare (`Parser.hs`)

Gramatica:

```
<lambda>   ::= <abs> | <app>
<abs>      ::= '\' <variable> '.' <lambda>
<app>      ::= <atom> (' ' <atom>)*
<atom>     ::= '(' <lambda> ')' | <variable> | <macro>
<variable> ::= [a-z]+
<macro>    ::= [A-Z0-9]+        (adăugat pentru sarcina 3)
```

Trebuie să folosim `newtype`-ul dat:

```haskell
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }
```

Un parser este o funcție de la intrare la „ori am eșuat (Nothing), ori iată
rezultatul și restul intrării (Just (a, rest))".

### Instanțe — fă-l o monadă cu alegere

Acesta este cod standard (boilerplate), dar esențial. Odată ce ai `Functor`,
`Applicative`, `Monad` și `Alternative`, poți scrie parsere în notație `do` și
le poți combina cu `<|>`.

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

Lucrul critic despre `<|>`: când `p1` eșuează, `p2` este rulat pe șirul
**original** `s`, niciodată pe ceea ce se uita `p1` în momentul în care a
renunțat. Aceasta este „revenirea automată" (backtracking) și este ceea ce ne
permite să scriem `bindingP <|> evalP` fără să ne facem griji privind consumul
parțial.

### Combinatori primitivi

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
    (c:rs) | f c -> Just (c, rs)
    _            -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (== c)

variableP :: Parser String
variableP = some (satisfy isLower)         -- una sau mai multe litere mici

macroP :: Parser String
macroP = some (satisfy (\c -> isUpper c || isDigit c))   -- una sau mai multe majuscule/cifre
```

`some p` (din `Alternative`) înseamnă „unul sau mai mulți `p`"; `many p`
înseamnă „zero sau mai mulți". Le obținem gratuit din instanța `Alternative`.

### Combinatori de gramatică

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

Două observații:

1. `appP` este asociativ la stânga deoarece `foldl App first rest` construiește
   `App (App (App first a1) a2) a3 ...`. Deci `x y z` se parsează ca `((x y) z)`.
2. `absP` consumă întregul corp prin `lambdaP`. De aceea `\x.x y` se parsează ca
   `\x.(x y)` — corpul înghite lacom tot ce urmează până la următoarea `)` sau
   până la sfârșitul intrării.

### 2.1 / 3.2 `parseLambda`

Punctul de intrare de nivel înalt. Rulează `lambdaP` pe intrare și întoarce
lambda. Cazul macro-ului este deja inclus în `atomP`, deci aceeași funcție
merge și pentru sarcina 3.2.

```haskell
parseLambda :: String -> Lambda
parseLambda s = case parse lambdaP s of
    Just (l, _) -> l
    Nothing     -> error ("parseLambda: cannot parse " ++ s)
```

Testele alimentează doar intrări valide, așa că un `error` la eșec este în
regulă aici. Întoarcem `l` chiar dacă mai există intrare rămasă — `parseLine`
este mai strict în privința asta.

---

## Partea 3 — Macro-uri și cod (`Code.hs` + `Parser.hs`)

### 3.1 `simplifyCtx` — evaluează o expresie care poate conține macro-uri

Două faze:
1. **Expandează** fiecare `Macro` la expresia pe care o reprezintă, căutând-o în
   context. Dacă un macro lipsește, întoarce `Left m`.
2. **Simplifică** expresia fără macro-uri folosind `Lambda.simplify`.

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
    Just l  -> expand ctx l    -- recursiv: definițiile pot conține macro-uri
    Nothing -> Left m

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step e = do
    e' <- expand ctx e
    return (simplify step e')
```

Observă cât de curat face monada `Either` acest lucru: fiecare `<-` se
scurtcircuitează la `Left` la primul macro lipsă. Nu sunt necesare instrucțiuni
`case` imbricate.

Indiciul din temă („putem refolosi `simplify`?") trimite exact aici: odată ce
macro-urile au dispărut, logica existentă tratează totul.

`normalCtx` și `applicativeCtx` sunt funcții de o linie care fixează strategia
de pas.

### 3.2 Parsarea macro-urilor

Deja realizată în `atomP` mai sus (ramura `Macro <$> macroP`). Parser-ul acceptă
orice secvență nevidă de litere majuscule sau cifre drept macro.

### 3.3 `parseLine` — o linie este ori o evaluare, ori o legare (binding)

```haskell
data Line = Eval Lambda
          | Binding String Lambda
```

O legare are forma `NUMEMACRO=<lambda>`. O evaluare este doar o lambda.

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

Încearcă întâi `bindingP`. Dacă intrarea începe cu un nume de macro, dar
caracterul următor nu este `=`, `bindingP` eșuează și `<|>` revine la șirul
original, apoi `Eval <$> lambdaP` încearcă din nou de la zero. Așa se face că
`X` singur se parsează ca `Eval (Macro "X")`, în timp ce `X=y` se parsează ca
`Binding "X" (Var "y")`.

Spre deosebire de `parseLambda`, aici cerem ca *întreaga* intrare să fie
consumată — o rămășiță la final înseamnă că utilizatorul a scris ceva greșit.

---

## Partea 4 — De Bruijn (`DeBruijn.hs`)

Rostul indicilor De Bruijn: variabilele legate devin numere („câți legători mai
sus"), așa că alfa-echivalența devine egalitate structurală și nu mai există
nicio capturare de variabile de care să-ți faci griji. Prețul este că trebuie să
te joci cu indicii când împingi valori peste legători.

```haskell
data DeBruijn = DBVar Int
              | DBFree String
              | DBApp DeBruijn DeBruijn
              | DBAbs String DeBruijn   -- numele păstrat doar ca indiciu
```

### 4.1 `toDB` — convertește numele în indici

Cărăm un `Context = [String]`: lista legătorilor aflați curent în domeniu, **cel
mai interior primul**. Când vedem un `Var x`, îl căutăm după poziție; dacă
lipsește, este liber.

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

(În fișierul efectiv, `where`-ul este inline: `toDB (x : ctx) e`.)

Când intrăm într-un `Abs x`, îl punem pe `x` în fața contextului, astfel încât
să devină indicele `0` pentru corp. Macro-urile sunt tratate ca variabile
libere, conform specificației.

### 4.2 `fromDB` — convertește indicii înapoi în nume

Direcția inversă. Contextul ne spune ce nume corespunde fiecărui indice.
Folosim indiciul de nume din `DBAbs` pentru a reconstrui numele legătorilor.

```haskell
fromDB :: Context -> DeBruijn -> Lambda
fromDB _   (DBFree x)    = Var x
fromDB ctx (DBVar i)     = Var (ctx !! i)
fromDB ctx (DBApp e1 e2) = App (fromDB ctx e1) (fromDB ctx e2)
fromDB ctx (DBAbs x e)   = Abs x (fromDB (x : ctx) e)
```

`toDB [] . fromDB []` este identitatea pe termeni bine formați.

### 4.3 `isNormalForm` — aceeași formă ca versiunea cu nume

```haskell
isNormalForm :: DeBruijn -> Bool
isNormalForm (DBVar _)             = True
isNormalForm (DBFree _)            = True
isNormalForm (DBAbs _ e)           = isNormalForm e
isNormalForm (DBApp (DBAbs _ _) _) = False
isNormalForm (DBApp e1 e2)         = isNormalForm e1 && isNormalForm e2
```

### 4.4 `reduce` — beta-reducerea De Bruijn

Aici apare aritmetica indicilor. Două funcții ajutătoare:

- **`shift d c e`**: adaugă `d` la fiecare indice din `e` care este `>= c`.
  Indicii liberi (cei nelegați în interiorul lui `e`) sunt deplasați; cei legați
  nu.
- **`subst j v e`**: înlocuiește `DBVar j` cu `v` peste tot în `e`. Indicii mai
  mari decât `j` sunt decrementați cu 1 (deoarece un legător a fost „consumat").
  Când traversăm într-un `DBAbs`, atât `j`, cât și pragul (cutoff) pentru
  deplasarea lui `v` cresc cu unu.

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

Verificare rapidă de bun-simț:

- `reduce a (DBVar 0)` = `a` — variabila legată devine valoarea. Bine.
- `reduce a (DBVar 1)` = `DBVar 0` — acel indice arăta *dincolo* de lambda
  exterioară; cu lambda dispărută, ar trebui să arate cu un pas mai puțin
  departe.
- `reduce a (DBAbs y (DBVar 1))` = `DBAbs y a` — indicele 1 din interiorul
  abstracției interioare înseamnă „un legător mai sus", ceea ce dinăuntru este
  *dincolo* de legătorul interior și se potrivește cu cel exterior. După
  substituirea și deplasarea lui `a` (niciun indice liber, deci deplasarea nu
  schimbă nimic), obținem `DBAbs y a`.

Niciun nume nefolosit, nicio detecție de capturare — doar contabilitate.

### 4.5 / 4.6 `normalStep` și `applicativeStep`

Mecanic identice cu versiunile cu nume, doar folosind `DBApp`/`DBAbs`.

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

Aceasta este răsplata: strategiile sunt practic identice cu codul cu nume, dar
`reduce` este acum trivial.

### 4.7 `simplify` — iterează până la FN

```haskell
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify step e
    | isNormalForm e = [e]
    | otherwise      = e : simplify step (step e)

normal      = simplify normalStep
applicative = simplify applicativeStep
```

Literalmente o copie a versiunii cu nume, cu tipul schimbat.

---

## Punând totul cap la cap

Când rulezi REPL-ul (`make repl && ./repl`):

1. Utilizatorul tastează o linie.
2. `parseLine` produce ori un `Binding` (adăugat în context), ori un `Eval l`
   (o lambda de redus).
3. Pentru `Eval`, `normalCtx ctx l` expandează macro-urile prin `expand`, apoi
   `simplify normalStep` produce urma (trace-ul) reducerii.
4. Urma este afișată; prompt-ul revine.

Fiecare pas folosește cod pe care l-am scris noi. Cele patru părți se compun
curat deoarece fiecare strat (substituție, parsare, macro-uri, De Bruijn) a fost
păstrat mic și ortogonal.

## Lucruri care m-au încurcat — ca să nu te încurci tu

- **Ordinea în `newVar`.** Comprehensiunea de listă naivă generează numele în
  ordinea greșită. Pune caracterul de la început în generatorul exterior.
- **Evitarea capturării în `reduce`.** Este ușor să uiți cazul de umbrire
  `v == x` sau să calculezi incomplet „numele de evitat". Include `freeVars
  body`, `freeVars e2` și `[x]`.
- **Semantica lui `<|>`.** Revine la șirul original. De aceea `bindingP <|>
  evalP` funcționează chiar dacă `bindingP` poate consuma numele macro-ului
  înainte de a eșua pe `=`.
- **`appP` este asociativ la stânga.** Folosește `foldl App`, nu `foldr`.
- **Pragul (cutoff) lui `shift` la De Bruijn.** Când `subst` intră într-o
  abstracție, atât adâncimea substituției, cât și pragul deplasării cresc cu unu.
- **Ordinea pattern-urilor în `isNormalForm`.** Cazul redex-ului
  `App (Abs _ _) _` trebuie să vină *înaintea* cazului generic `App`.

---

## Cum rulezi fiecare parte și ce comenzi trebuie să știi

### Compilare și rulare prin `Makefile`

Toate comenzile se dau din directorul care conține `Makefile` (adică
`skel-solved/`).

```sh
make            # alias pentru `make repl`
make repl       # compilează interpretorul -> binarul ./repl
make test       # compilează testele -> ./run_tests, apoi le rulează pe toate
make clean      # șterge build/, repl și run_tests
```

Detalii utile despre `Makefile`:

- Flag-urile sunt `-Wall` (plus câteva warning-uri dezactivate, ca să nu te
  asurzească).
- Toate fișierele intermediare `.o` / `.hi` ajung în directorul `build/`, ca
  arborele sursă să rămână curat. De aceea `make clean` doar șterge `build/`.
- `SRCS` (sursele compilate pentru REPL) sunt:
  `Lambda.hs Code.hs Parser.hs Default.hs DeBruijn.hs`.

> **Atenție:** în folder există deja `repl.exe` și `run_tests.exe` — sunt binare
> precompilate pentru **Windows** și nu rulează pe Linux. Pe Linux îți compilezi
> propriile binare cu `make repl` / `make test` (vor apărea ca `./repl` și
> `./run_tests`, fără extensie).

### Rularea REPL-ului (`./repl`)

```sh
make repl
./repl
```

Vei vedea prompt-ul `λ> `. La fiecare linie, interpretorul (vezi `main.hs`)
face una din următoarele:

| Ce tastezi          | Ce se întâmplă                                              |
|---------------------|------------------------------------------------------------|
| `:q`                | iese din REPL                                              |
| `:r`                | resetează contextul la `defaultContext`                    |
| `:ctx`              | afișează toate legările (binding-urile) din context        |
| `NUME=<lambda>`     | adaugă o legare nouă în context (nu afișează nimic)        |
| `<lambda>`          | evaluează expresia cu `normalCtx` și afișează urma reducerii|

Reducerea în REPL se face mereu în **ordine normală** (`normalCtx`). Macro-urile
predefinite din `defaultContext` (vezi `Default.hs`) sunt:
`M`, `I`, `K`, `KI`, `C`, `Y`.

În REPL scrii lambda cu **un singur backslash** (linia se citește brut):

```text
λ> I x
((λx.x) x)
x
λ> ID=\x.x
λ> ID y
((λx.x) y)
y
λ> :ctx
ID = λx.x
M = λx.(x x)
I = λx.x
K = λx.λy.x
KI = λx.λy.y
C = λx.λy.λz.((x z) y)
Y = λf.(λx.(f (x x)) λx.(f (x x)))
λ> :q
```

### Rularea testelor (`./run_tests`)

Cel mai simplu, prin `make`:

```sh
make test       # compilează și rulează TOATE grupurile de teste
```

Sau, dacă binarul `run_tests` e deja compilat, îl poți rula direct și poți alege
un singur grup (vezi `Tests/Main.hs`):

```sh
./run_tests              # toate testele
./run_tests lambda       # doar Partea 1 (Lambda.hs)
./run_tests parser       # doar Partea 2 (Parser.hs)
./run_tests code         # doar Partea 3 (Code.hs)
./run_tests debruijn     # doar Partea 4, extra (DeBruijn.hs)
```

Orice alt argument afișează mesajul de utilizare:
`Usage: run_tests [lambda|parser|code|debruijn]`.

### Experimentare în GHCi

GHCi e cel mai bun loc să testezi funcțiile individual. Pornește-l cu `ghci`.

#### Punctul de intrare A — `main.hs` (Părțile 1, 2 și 3)

Încarcă `main.hs`: el importă `Lambda`, `Parser`, `Code` și `Default`, deci
îți aduce **tot** ce-ți trebuie pentru calculul cu nume într-un singur loc —
funcțiile de reducere, parser-ul și contextul implicit.

```text
ghci> :load main.hs
```

> **Gotcha la backslash:** în GHCi argumentele sunt șiruri Haskell, deci
> backslash-ul trebuie dublat: scrii `"\\x.x"`, nu `"\x.x"`. (În REPL-ul
> `./repl` scrii un singur backslash, fiindcă acolo linia se citește brut.)

Partea 1 — evaluare (toate funcțiile din `Lambda.hs`):

```text
ghci> vars (parseLambda "\\x.x y")
["x","y"]
ghci> freeVars (parseLambda "\\x.x y")
["y"]
ghci> newVar ["a","b","c"]
"d"
ghci> isNormalForm (parseLambda "(\\x.x) y")
False
ghci> normal (parseLambda "(\\x.x) y")          -- ordine normală
[((λx.x) y),y]
ghci> applicative (parseLambda "(\\x.x) y")     -- ordine aplicativă
[((λx.x) y),y]
ghci> reduce "x" (parseLambda "\\y.x y") (parseLambda "\\x.y")
λa.((λx.y) a)
```

Pentru expresii fără formă normală (de ex. Ω), lista e infinită — folosește
`take`:

```text
ghci> take 4 (normal (parseLambda "(\\x.x x) (\\x.x x)"))
[((λx.(x x)) (λx.(x x))),((λx.(x x)) (λx.(x x))),((λx.(x x)) (λx.(x x))),((λx.(x x)) (λx.(x x)))]
```

Partea 2 — parsare (`Parser.hs`):

```text
ghci> parseLambda "(\\x.x) y"
((λx.x) y)
ghci> parseLambda "x y z"                       -- asociativ la stânga
((x y) z)
```

Partea 3 — macro-uri și context (`Code.hs` + `Default.hs`):

```text
ghci> normalCtx defaultContext (parseLambda "K x y")
Right [(((λx.λy.x) x) y),((λy.x) y),x]
ghci> normalCtx defaultContext (parseLambda "FOO x")   -- macro inexistent
Left "FOO"
ghci> expand defaultContext (parseLambda "I")
Right λx.x
ghci> parseLine "ID=\\x.x"
Right ID = λx.x
ghci> parseLine "x y"
Right (x y)
```

#### Punctul de intrare B — `DeBruijn.hs` (Partea 4)

`DeBruijn` definește propriile `normal`, `simplify`, `reduce`, `isNormalForm`
etc., care **se ciocnesc** cu cele din `Lambda`. De aceea îl ții separat. Cel
mai comod e să încarci și `Parser` (ca să folosești `parseLambda` în loc să
construiești termenii de mână):

```text
ghci> :load Parser.hs DeBruijn.hs
ghci> :module *DeBruijn            -- pune scope-ul pe DeBruijn (normal/toDB/... nemarcate)
ghci> import Parser (parseLambda)  -- adaugă parseLambda peste
```

Apoi:

```text
ghci> toDB [] (parseLambda "\\x.x")
λ 0
ghci> toDB [] (parseLambda "(\\x.x) y")
((λ 0) y)
ghci> normal (toDB [] (parseLambda "(\\x.x) y"))      -- DeBruijn.normal
[((λ 0) y),y]
ghci> map (fromDB []) (normal (toDB [] (parseLambda "(\\x.x) y")))
[((λx.x) y),y]
```

(Reține: instanța `Show` pentru `DeBruijn` afișează legătorii ca `λ ` fără nume
și variabilele legate ca indici.)

### Comenzi GHCi de bază pe care merită să le știi

| Comandă               | La ce e bună                                                 |
|-----------------------|--------------------------------------------------------------|
| `:load Fis.hs` / `:l` | încarcă un fișier (și dependențele lui)                      |
| `:reload` / `:r`      | reîncarcă după ce ai editat sursa                           |
| `:type expr` / `:t`   | afișează tipul unei expresii (de ex. `:t reduce`)           |
| `:info Nume` / `:i`   | afișează definiția / instanțele / fixity-ul                 |
| `:browse Modul`       | listează tot ce exportă un modul (de ex. `:browse Lambda`)  |
| `:module *M` / `:m`   | comută scope-ul pe modulul `M` (cu `*` vezi și ce nu exportă)|
| `import M (f, g)`     | aduce nume punctuale în scope după `:load`                  |
| `:set +s`             | afișează timpul și memoria după fiecare evaluare            |
| `:quit` / `:q`        | iese din GHCi                                               |

Alternativ poți porni GHCi direct cu fișierul: `ghci main.hs`. Dacă editezi
sursa, dă `:r` în loc să reîncarci de la zero.
