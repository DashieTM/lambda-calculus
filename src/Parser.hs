{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Parser where

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])
  --deriving Show

parse :: Parser a -> String -> [(a,String)]
parse (P parser) xs = parser xs

item :: Parser Char
item = P (\case
         [] -> []
	 (x:xs) -> [(x,xs)])


string :: Parser [Char]
string = P (\case 
           [] -> []
           xs -> [(xs,[])])

digits :: Parser Char
digits = use isDigit

alpha :: Parser Char
alpha = use isAlpha

space :: Parser [Char]
space = many $ use isSpace

strVar :: Parser [Char]
strVar = someOf alpha

symbol :: Char -> Parser Char
symbol ch = P (\case
              [] -> []  
              (x:xs) -> if x == ch then [(x,xs)] else [])

someOf :: Parser Char -> Parser [Char]
someOf p = P (\input -> case parse p input of
                     [] -> []
                     [(x,xs)] -> let p1 = parse ( someOf p) xs in
                                 case p1 of
                                 [] -> [(x:[],xs)]
                                 [(y,ys)] -> let p2 = head p1 in
                                             [(x:(fst p2),(snd p2))])


applyRec :: Parser a -> Parser [a]
applyRec  x = applyXTimes x <|> pure []

applyXTimes :: Parser a -> Parser [a]
applyXTimes x = pure (:) <*> x <*> applyRec x


instance Functor Parser where
  --fmap :: (a -> b) -> Parser a -> Parser a
  fmap f parser = P (\input -> case parse parser input of
                               [] -> []
	                       [(x,xs)] -> [(f x,xs)])

instance Applicative Parser where
  pure x = P (\xs -> [(x,xs)])
  parser1 <*> parser2 = P (\input -> case parse parser1 input of
                                    [] -> []
                                    [(x,xs)] -> parse (fmap x parser2) xs)

instance Monad Parser where
 parser >>= f = P (\input -> case parse parser input of
                             [] -> []
                             [(x,xs)] -> parse (f x) xs )

instance Alternative Parser where
  empty = P (\input -> [])

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\input -> case parse p1 input of
                           [] -> parse p2 input
                           [(x,y)] -> [(x,y)])

use :: (Char -> Bool) -> Parser Char
use check = do
            x <- item
            if check x then return x else empty  
