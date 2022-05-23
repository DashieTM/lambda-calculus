module Lambda_interpreter where

-- Lambda_Int (*)

import Data.Char
import qualified Data.Text as T
import Data.Bits (Bits(xor))
import Control.Concurrent (yield)

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions 
    | App Term Term  -- Applications
    | Empty
    deriving (Show, Eq, Read)

definedVars :: [(Term,Term)]
definedVars = [((Var "test"),(Abs "hey" (Var "hey")))]

-- rename lambda
alphareduce (Var x) = (Var x)

alphareduce (App (Abs x y) (Var z))
    | isInsideAlp (Var z) (Abs x y) = (App (Abs x y) (Var ((++) z "1")))
    | otherwise = (App (Abs x y) (Var z))

alphareduce (App x y) = (App (alphareduce x) (alphareduce y))
alphareduce (Abs x y) = (Abs x (alphareduce y))


-- parse parameter into lambda
betareduce (Var x) = (Var x)
betareduce (Abs x y) = (Abs x (betareduce y ))
betareduce (App (Abs x y) z) = (substitute (Var x) z y)
betareduce (App x y) = (App x y)


-- variables to calculation
deltareduce x
    | frs (containsVars x) = do
        let var = sec (containsVars x)
        substitute (frs var) (sec var) x
    | otherwise = x

deltest x =  (containsVars x)

substitute :: Term -> Term -> Term -> Term
substitute (Var a) b (Var c)
    | c == a = b
    | otherwise = (Var c)

substitute (Var a) b (Abs c d)
    | (isInsideSub (Var a) d) = (Abs c (substitute (Var a) b d))
    | otherwise = (Abs c d)

substitute (Var a) b (App c d) = (App (substitute (Var a) b c ) (substitute (Var a) b d ))


isBeta (App (Abs x y) z) = True
isBeta (App (App x y) z) = True
isBeta _ = False

isBound (App (Abs x y) (Var z)) = x == z

isInsideSub :: Term -> Term -> Bool

isInsideSub (Var x) (App (Var a) b)
    | x == a = True
    | otherwise = False

isInsideSub (Var x) (App a b) = isInsideSub  (Var x) a || isInsideSub  (Var x) b

isInsideSub (Var x) (Abs a (Var b)) = False

isInsideSub (Var x) (Abs a b) = isInsideSub  (Var x) b

isInsideSub (Var x) (Var a) = False

isInsideAlp :: Term -> Term -> Bool

isInsideAlp (Var x) (App a (Var b))
    | x == b = True
    | otherwise = False

isInsideAlp (Var x) (App a b) = isInsideAlp (Var x) a || isInsideAlp (Var x) b

isInsideAlp (Var x) (Abs a (Var b)) = False

isInsideAlp (Var x) (Abs a b) = isInsideAlp (Var x) b

isInsideAlp (Var x) (Var a) = False

isInsideDel :: Term -> Term -> Bool

isInsideDel (Var x) (App a b) = isInsideDel (Var x) a || isInsideDel (Var x) b

isInsideDel (Var x) (Abs a b) = isInsideDel (Var x) b

isInsideDel (Var x) (Var a)
    | x == a = True
    | otherwise = False

isVar (Var x) = True
isVar x = False

frs (a,b) = a
sec (a,b) = b


contains :: Term -> [(Term,Term)] -> (Bool,(Term,Term))
contains _ [] = (False,(Var "-1", Var "-1"))
contains n (x:xs)
  | isInsideDel (frs x) n = (True,x)
  | otherwise = contains n xs

containsVars n = contains n definedVars


-- parse a lambda
reduce :: Term -> Term
reduce x
    | isBeta (betareduce (alphareduce (deltareduce x))) = reduce (betareduce (alphareduce (deltareduce x)))
    | otherwise = (betareduce (alphareduce (deltareduce x)))

convert :: String -> IO ()
--convert x = print (reduce(head(parse (tokenize x))))
convert x = print (head(parse (tokenize x)))
--convert x = print (tokenize x)

data Token = TokAbs
           | TokApp
           | TokVar
           | Tokright
           | Tokleft
           | TokString Id
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | c == '"'  = tokenizeString cs
    | c == '('  = Tokleft : tokenize cs
    | c == ')'  = Tokright : tokenize cs
    | isAlpha c = tokenizeTerm ([c]++cs)
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

checkChar :: Char -> Bool
checkChar x
    | x == 'A' || x == 'V' = True
    | otherwise = False

tokenizeString :: String -> [Token]
tokenizeString [] = []
tokenizeString (x : xs)
    | isAlpha x = do
            let str = ([x] ++ checkString xs)
            let len = (length str) - 1
            (TokString str) : tokenizeString (drop len xs)
    | x == '"'  || x == '\\' = tokenize xs
    | otherwise = error $ "Cannot tokenize -> missing quote after" ++ [x]


checkString [] = []
checkString (x : xs)
    | isAlpha x = [x] ++ checkString xs
    | otherwise = []

tokenizeTerm :: [Char] -> [Token]
tokenizeTerm [] = []
tokenizeTerm (x : xs)
    | x == 'A' && head xs == 'p' && (head (drop 1 xs)) == 'p' = TokApp : tokenize (drop 2 xs)
    | x == 'A' && head xs == 'b' && (head (drop 1 xs)) == 's' = TokAbs : tokenize (drop 2 xs)
    | x == 'V' && head xs == 'a' && (head (drop 1 xs)) == 'r' = TokVar : tokenize (drop 2 xs)
    | otherwise = error $ "Cannot tokenize -> not a term" ++ [x]

len :: [Int]
len = []

parse :: [Token] -> [Term]
parse [] = []
parse (TokString x : xs) = [Var x]
parse (TokApp : xs) = [App (head (parse xs)) (head (parse (drop (head len) xs)))]
parse (x : xs)
    | x == TokApp = do
        let len = [depth xs 0 0]
        [App (head (parse xs)) (head (parse (drop (head len) xs)))] --where len = [depth xs 0 0]
    | x == TokAbs = [Abs (getId (head xs)) (head(parse (drop 1 xs)))]
    | otherwise = if (isEmpty xs) then [] else parse xs

getId :: Token -> Id
getId (TokString x) = x
getId _ = error $ "Abs needs a string as variable"

depth :: [Token] -> Int -> Int -> Int
depth [] y z = y
depth (x : xs) y z
    | x == Tokleft && z > 1 = depth xs (y + 1) (z - 1)
    | x == Tokleft && z == 1 = y + 1
    | x == Tokright = depth xs (y + 1) (z + 1)
    | otherwise = depth xs (y + 1) z

isEmpty :: [Token] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

-- App ::= (App (Var) (Var)) | (App (Abs) (Abs)) | 



-------------------------------------------
----tests

--(App (App (Var "a") (Var "b")) (Var "c"))

-------------------------------------------
-- `(λx.x) a` reduces to `a`
-- >>> reduce (App (Abs "x" (Var "x")) (Var "a")) == (Var "a")
-- True

-- `(λx.x x) (λx.x)` reduces to `λx.x`
-- >>> reduce (App (Abs "x" (App (Var "x") (Var "x"))) (Abs "x" (Var "x"))) == (Abs "x" (Var "x")) 
-- True

-- `(λx.λy.x y) y` reduces to `λy1.y y1`, and not `λy.y y`
-- >>> reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y")) == (Abs "y1" (App (Var "y") (Var "y1")))
-- True

-- Note: The above test case is too britle since its success depends on the choice of the fresh variable `y1` chosen to avoid variable capture.
-- To make the test more robust, it would be better to check that the two terms are alpha equivalent instead. 
-- Note: Checking if two terms are alpha equivalent is not part of the minimum goal.

-- `(λx.λy.x y) y` reduces to a term that is alpha equivalent to `λz.y z`
-- >>> alphaEq (reduce (App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) (Var "y"))) (Abs "z" (App (Var "y") (Var "z")))
-- True 


--Notes and Hints (will be continuously updated):
--In case you can't figure out how to start implementing beta reduction, I recommend that you look at how this is done in the lecture notes for inspiration. Here is a rough suggestion on how to start:
--
--Implement a function freeVars t that returns a set of all free variables within a lambda term t. You may want to represent sets as lists without duplicates. Appendix A.2 of the lecture notes contains recursive definitions of non-freeness that you may find useful.
--Implement a function substitute (x,tx) t that replaces all free occurrences of the variable x within the term t with the term tx. Take care to avoid capturing substitutions (you will have to do some alpha renaming with fresh variables to avoid this). Appendix A.2 of the lecture notes contains a recursive definition of substitution that you may find useful. In case you find the task of avoiding variable capture too challenging, skip this and only use terms with unique bound and free variable names.
--Implement a function isBetaRedex t which returns True iff the top level of the term t is a beta redex.
--Use substitute to implement a function betaReduce t that applies a beta reduction to top level of the term t.
--You can now combine the functions just defined to define new functions such as:
--leftmostOutermost :: Term -> Term, leftmostInnermost :: Term -> Term that perform a single reduction step using the appropriate evaluation strategy.
--derivation :: (Term -> Term) -> Term -> [Term] that performs a step by step derivation using a given reduction strategy.
--reduce :: Term -> Term that uses returns the beta normal form of a given term in case it exists. (Minimal Goal)


------------------------------------------------

-- I had a hard time understanding how exactly lambda calculus works
-- (problematic was also the app implementation, which is essentially just brackets....)
-- below you can find all the different variations that I have tried
-- At least the basics should now work

--reduce (App (Abs a b) (Abs c d))
--    | isBeta (App (Abs a b) (Abs c d)) = (App (Abs a b) (Abs c d))
 --   | isBound (App (Abs x y ) z) = reduce (betareduce (alphareduce (App (Abs x y ) z)))
--    | otherwise = reduce (betareduce (alphareduce (App (Abs a b) (Abs c d))))

--reduce (App (Abs x y) (App (Var a) (Var b)))
--    | isBeta (App (Abs x y) (App (Var a) (Var b))) = (App (Abs x y) (App (Var a) (Var b)))
 --   | isBound (App (Abs x y ) z) = reduce (betareduce (alphareduce (App (Abs x y ) z)))
--    | otherwise = reduce (betareduce (alphareduce (App (Abs x y) (App (Var a) (Var b)))))

--reduce (App (Abs x y) z)
 --   | isBeta (App (Abs x y) z) = (App (Abs x y) z)
 --   | isBound (App (Abs x y ) z) = reduce (betareduce (alphareduce (App (Abs x y ) z)))
--    | otherwise = reduce (betareduce (alphareduce (App (Abs x y) z)))

--reduce (App (Var z) (Abs x y))
--    | isBeta (App (Var z) (Abs x y)) = (App (Var z) (Abs x y))
--    | isBound (App (Var z) (Abs x y)) = reduce (betareduce (alphareduce (App (Var z) (Abs x y))))
--    | otherwise = reduce (betareduce (alphareduce (App (Var z) (Abs x y))))

--reduce (App (Var z) (App x y))
--    | isBeta (App x y) = (App (Var z) (App x y))
--    | otherwise = reduce (App (Var z) (reduce(App x y)))



--reduce (Var x Var y) = (App (Var x) (Var y))
--reduce (App (Var x) (Var y)) = 
--reduce (App (Abs x y) (Abs w z))
--reduce (App (App x y) (App w z))
--reduce (App x y) = reduce x

--isBeta (App (Abs a b) (Abs c d)) = False
--isBeta (App (Abs a b) (App (Var c) (Var d))) = False
--isBeta (App (Abs a b) c) = False
--isBeta (App x y) = isBeta x && isBeta y
--isBeta (Abs x (Var y)) = True
--isBeta (Abs x (Abs y z)) = False
--isBeta (Abs z (App (Var x) (Var y))) = True

--isBeta (Abs x y)
--    | isVar y = True
--    | otherwise = isBeta y

--leftmostOutermost (Var x) = (Var x)
--leftmostOutermost (Abs x y) = (Abs x (betareduce (alphareduce y)))
--leftmostOutermost (App (Abs x y) z) = (betareduce (alphareduce (App (Abs x y) z)))

--substitute (Var a) (Var b) (Abs x y)
--    |  y == (Var x) = (Var b)
--    | (Var a) == (Var x) = (substitute (Var a) (Var b) y)
--    | otherwise = (Abs b (substitute (Var a) (Var b) y))

--substitute (Var a) (Abs b c) (Abs x y)
--    | (Var a) == y = (Abs x (Abs b c))
--    | otherwise = (Abs x (substitute (Var a) (Abs b c) y))

--substitute (Var a) (App b c) (Abs x y)
--    | (Var a) == y = (Abs x (App b c))
--   | otherwise = (Abs x (substitute (Var a) (App b c) y))

--substitute (Var a) (Var b) (App x y)
--    | (Var a) == x && (Var a) == y = (App (Var b) (Var b))
--    | (Var a) == x = (App (Var b) (substitute (Var a) (Var b) y))
--    | (Var a) == y = (App (substitute (Var a) (Var b) x) (Var b))
--    | otherwise = (App (substitute (Var a) (Var b) x)(substitute (Var a) (Var b) y))

--substitute (Var a) (Abs b c) (App x y)
--    | (Var a) == x && (Var a) == y = (App (Abs b c) (Abs b c))
--    | (Var a) == x = (App (Abs b c) (substitute (Var a) (Abs b c) y))
--    | (Var a) == y = (App (substitute (Var a) (Abs b c) x) (Abs b c))
--    | otherwise = (App (substitute (Var a) (Abs b c) x) (substitute (Var a) (Abs b c) y))

--substitute (Var a) (App b c) (App x y)
--    | (Var a) == x && (Var a) == y = (App (App b c) (App b c))
--    | (Var a) == x = (App (App b c) (substitute (Var a) (App b c) y))
--    | (Var a) == y = (App (substitute (Var a) (App b c) x) (App b c))
--  

--betareduce (Abs a (Abs b (App (Var c) (Var d) )))
--    | (Var a) == (Var c) = (Abs a (App (Var a) (Var d)))
--    | (Var a) == (Var d) = (Abs a (App (Var a) (Var c)))
--    | otherwise = (Abs a (Abs b (App (Var c) (Var d) )))
--betareduce (App (Abs x y) (App (Var a) (Var b))) = (App (substitute (Var x) (Var a) (Abs x y)) (Var b))
--betareduce (App (Abs x (Var y)) (Var z)) = (substitute (Var x) (Var z) (Abs x (Var y)))
--betareduce (App (Abs x (Var y) ) z)
--    | isBeta z = (substitute (Var x) z (Abs x (Var y)))
--    | otherwise = reduce (App (Abs x (Var y)) (reduce z) )
--betareduce (App (Abs x y) (Var z)) = ((substitute (Var x) (Var z) (Abs x y)))
--betareduce (App (Abs a b) (Abs c (Var d)))
--    | (Var c) == (Var d) && isInside c b = (Abs c (Var d))
--    | otherwise =  (App (Abs a b) (reduce(Abs c (Var d))))
--betareduce (App (Abs x y) z)
--    | isBeta z && isInside x y = (substitute (Var x) z (Abs x y))
--    | otherwise = (substitute (Var x) z (Abs x y))
--    | isBeta z && (not (isInside x y)) = (substitute (Var x) z (Abs x y))
--    | otherwise = reduce (App (Abs x y) (reduce z) )
--betareduce (App (Var z) (Abs x (Var y))) = (App (Var z) (reduce (Abs x (Var y))))
--betareduce (App (Var z) (Abs x y)) = (App (Var z) (reduce (Abs x y)))

--alphareduce (App (Var z) (Abs x (Var y) )) = (App (Var z) (Abs x (Var y)))
--alphareduce (Abs z (App (Var x) (Var y)))
--    | (Var z) == (Var x) && (Var x) == (Var y) = (Abs ((++) z "1") (App (Var ((++) z "1")) (Var y)))
--    | (Var z) == (Var y) && (Var x) == (Var y) = (Abs ((++) z "1") (App (Var x) (Var ((++) z "1"))))

--alphareduce (App (Abs x (Var y)) (Var z))
--    | (Var x) == (Var z) = (App (Abs ((++) x "1") (Var y)) (Var z))
--    | otherwise = (App (Abs x (Var y)) (Var z))

--reduce (App (Abs x y) z) = reduce (leftmostOutermost (App (Abs x y) z))

--reduce (App x y)
--    | isBeta x || isBeta y = reduce (App (reduce x) (reduce y))
--    | otherwise = (App x y)


--leftmostOutermost (App x y)
--    | isBeta x = (App (betareduce (alphareduce x)) y)
--    | isBeta y = (App x (betareduce (alphareduce y)))
--    | otherwise = (betareduce (alphareduce (App x y)))
--leftmostOutermost x = (betareduce (alphareduce x))
--parseSingle :: Token -> Term
--parseSingle (TokString x) = (Var x)
--parseSingle (TokApp) = (App )

--lookAhead ::[Token] -> Bool
--lookAhead (x:xs)
--    | x == TokAbs = True
--    | x == TokApp = True
--    | x == TokVar = True
--    | x == Tokleft = True
--    | x == Tokright = True
--    | x == TokPrep = True
--    | x == (TokString Id) = True
--    | otherwise = False


---- go ask fahrhad
---- how i parse, me no comprende
