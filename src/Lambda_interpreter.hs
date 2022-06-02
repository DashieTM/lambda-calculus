{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Lambda_interpreter where

-- Lambda_Int (*)

import Data.Char
import qualified Data.Text as T
import Data.Bits (Bits(xor))
import Control.Concurrent (yield)
import Control.Applicative
import Parser

type Id = String

data Term = Var Id   -- Variables
    | Abs Id Term    -- Abstractions 
    | App Term Term  -- Applications
    | Empty
    deriving ( Eq, Read)

instance Show Term where
  show (Abs x y) = concat ["λ",show x,".",show y]
  show (App x y) = concat ["(",show x, " " , show y,")"]
  show (Var x)   = show x
  show Empty = "Empty"

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

convert :: String -> String
--convert x = print (reduce(head(parse (tokenize x))))
--convert x = do
--        print (head(frs(parse (tokenize x )[] 0)))
--        print (sec(parse (tokenize x )[] 0))
--convert x = print (tokenize x)
--convert x = do
    --let tokenstring = tokenize x 
    --let depthoftree = checkTreeDepth tokenstring
    --show (reduce(head(parse tokenstring depthoftree 0)))
convert x = show $ reduce $ parseInput x

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
    -- x == 'L' = TokAbs : tokenize xs
    | x == 'V' && head xs == 'a' && (head (drop 1 xs)) == 'r' = TokVar : tokenize (drop 2 xs)
    | otherwise = error $ "Cannot tokenize -> not a term" ++ [x]

len :: [Int]
len = []


getId :: Token -> Id
getId (TokString x) = x
getId _ = error $ "Abs needs a string as variable"

depth :: [Token] -> Int -> Int -> Int
depth [] y z = y
depth (x : xs) y z
    | x == Tokright && z > 1 = depth xs (y + 1) (z - 1) --next time I have to make sure that I don't get stuck on left or right again, remember 2hours wasted because of this....
    | x == Tokright && z == 1 = y + 1
    | x == Tokleft = depth xs (y + 1) (z + 1)
    | otherwise = depth xs (y + 1) z

checkTreeDepth :: [Token] -> [Int]
checkTreeDepth [] = []
checkTreeDepth (x : xs)
    | x == TokApp = depth xs 0 0 : checkTreeDepth xs
    | otherwise = checkTreeDepth xs

isEmpty :: [Token] -> Bool
isEmpty [] = True
isEmpty (x:xs) = False

--parser extensions
-------------------------------------------
--token :: Parser a -> Parser a
token parser = do space
                  x <- parser
                  space
                  return x

--paren :: Parser a -> Parser a
paren parser  = do symbol '('
                   x <- parser
                   symbol ')'
                   return x
parseUnit :: Parser Term
parseUnit = parseLam <|> parseVar <|> paren parseTerm 

--parseLam :: Parser Token
parseLam = do token $ symbol 'λ'
              x <- token strVar
              token $ symbol '.'
              y <- token parseTerm
              return (Abs x y)
                


parseVar :: Parser Term
parseVar = Var <$> strVar--do x <- token strVar
           --   return (Var x)

parseTerm = do x <- applyXTimes parseUnit
               return (foldl1 App x)


parseInput str = case parse parseTerm str of
                 [] -> error $ str ++ " is not a lambda Term."
                 [(x,[])] -> x
                 [(x,xs)] -> error $ "Managed to parse: " ++ (show x) ++ " but not: " ++ xs
                 (x:xs)   -> error   "input is nonsense"  


-------------------------------------------

-------------------------------------------
----tests

--(App (App (Var "a") (Var "b")) (Var "c"))
--(App (Abs "a" (App (Var "s") (Var "d"))) (Abs "f" (Var "g")))

-------------------------------------------
