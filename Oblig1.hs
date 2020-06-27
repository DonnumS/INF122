module Oblig1 where
-- Sebastian Dønnum

import Data.Char (isSpace)
import Data.List
import Text.Parsec




-- A.(a)
fjern :: String -> Int -> String

fjern [] _      = []
fjern (x:xs) i
    | i == 0    = xs
    | otherwise = x : fjern xs (i-1)


-- A.(b)
fjernc :: String -> Char -> String

fjernc [] _    = []
fjernc xs c = [x | x <- xs, not (x == c)]


-- A.(c)
tegnpos :: String -> Char -> [Int]

tegnpos s c = [ x | (y, x) <- zip s [0..], y == c ]


-- B.(a)
ord :: String -> [String]

ord = unfoldr s
    where s [] = Nothing
          s xs = let (word, rest) = break isSpace xs
                    in Just (word, dropWhile isSpace rest)


-- B.(b)
{-
At one point i managed to get this working but i messed it up somehow and
can't seem to figure out what i did wrong.
-}
tokenize :: String -> String -> String -> [String]

tokenize str imp rem = case ys of
    [] -> words xs
    (z:zs) -> mempty xs z (tokenize zs imp rem)
    where
      (xs, ys) = break isSpace str


-- C

{-
Subset checks if list x is subset of list y. This is used both ways to ensure
that the lists dont have any extra elements that the other list does not have
Then eqli returns True if subset is True for both lists

Probably a way to do this by not making a different function called subset,
but i did not manage to do this
-}

subset [] y = True
subset (x:xs) y = elem x y && subset xs y


eqli :: Eq t => [t] -> [t] -> Bool
eqli x y = subset x y && subset y x


-- D
{-
Have not found a way to work in the function that removes letters. Made this to
work around the fact that bracketHelper is not workin when there are letters in
the string. Tried working around it by creating the removeLetters function, but
I did not manage to implement it into sjekk
-}
removeLetters :: String -> String
removeLetters xs = [ x | x <- xs, not (x `elem` ['a'..'z']) ]

bracketHelper = many brackets >> return ()
    where brackets = choice [ between ( char '(' ) ( char ')' ) bracketHelper, between ( char '[' ) ( char ']' ) bracketHelper, between ( char '{' ) ( char '}') bracketHelper]

sjekk :: String -> String
sjekk input = case parse (bracketHelper >> eof) "" input of
    Left _ -> "Feil!"
    Right _ -> "Korrekt!"
