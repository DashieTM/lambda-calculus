import Data.Bits (Bits(xor))
import Control.Concurrent (yield)


data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n)
 = pure n
eval (Div x y) = do
    fr <- eval x
    sc <- eval y
    safediv fr sc

--General Infos
{--- 
 - variables|functions in haskell need to start with a lowercase letter -> helloVariable 
 - Numbers -> 3int | Uppercase start -> Hellovariable are disallowed
 - Datatypes in haskell need to start with an Uppercase letter -> SomeData
 - Numbers -> 3data | Lowercase start -> someData are disallowed
 -
 -
 -
 -
 -
 -
 - ---}




type Something = Int
--this doesn't allow instances NOR | branches !
--note that type Something = Something int is also not allowed !!!
--instance Show Something where
--  show (Something x) = show x ILLEGAL!

newtype Something2 = Something2 Int
instance Show Something2 where
  show (Something2 x) = show x
--this allows instances but not | branches!

data Something3 = Empty | Something3 Int -- note that if we do not use Something3 again, then you have no way of using the Int.
instance Show Something3 where
  show (Empty) = "This is Empty"
  show (Something3 x) = concat ["this is a number",show x]
--these definitions are ridiculous, but they are made to understand the language better
--okay important:
-- !!!!!!!!
--data Something = Dataconstructor | Dataconstructor (Types) only the Types inside the brackets can actually be used to do anything
--the rest are just namespaces or rather as said data themselves.
--if you wanna do anything with it, it needs to be a type not a data constructor
-- !!!!!!!!

fold :: (a -> b -> b) -> b -> [a] -> b -- note that you can also define is as (a -> a -> a) -> a -> [a] -> a it would just mean that you no longer have a choice to make 2 different types work
fold f end [] = end
fold f end (x:xs) = x `f` (fold f end xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f end [] = end
foldl' f end (x:xs) = foldl' f (f end x) xs

--fold div 1 [1..10] would mean x `div` 0 at some point according
--to the definition.
--however if you look at foldl, then you see that it would never actually divide by 0.
--foldl therefore just ends with 0 as a result 0/something = 0.
 
incrementlist :: [Int] -> [Int]
incrementlist = map (\x -> x + 5) 

lettest = let x = 5 in
	  let y = 3 in
	  x + y

lambdatest = (\x -> (\y -> x + y )) 

curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f x y  =  f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f (x,y)  = f x y
