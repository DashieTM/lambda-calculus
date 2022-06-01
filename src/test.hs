fib = 1 : 1 : [a + b | (a,b) <- zip fib (tail fib) ]
showfib = take 200 fib

data Maybe2 a = Just2 a | Nothing2 deriving Show


instance Functor Maybe2 where
fmap2 f (Just2 a) = Just2 (f a)
fmap2 f (Nothing2) = Nothing2

instance Applicative Maybe2 where
--run :: Maybe2 f => a -> f a
run = Just2
checkout (Just2 a) = a
checkout Nothing2 = error $ "this shit is not a number dude !!!"
Just2 f <<*>> j = fmap2 f j
Nothing2 <<*>> j = Nothing2

instance Monad Maybe2 where
Just2 a >>>= f = f a
Nothing2 >>>= f = Nothing2

