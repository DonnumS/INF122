-- Sebastian Dønnum
module Oblig2 where

import Data.Char

--Expr -> Int | - Expr | + Expr Expr | * Expr Expr | Var
--Int -> Digit | Digit Int
--Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9

data Ast = Tall Int | Sum Ast Ast | Mult Ast Ast | Min Ast | Var String  deriving (Eq, Show)
-- denne definisjonen av Ast utvides med manglende bitene for Mult og Min

-- Tokenizer. Modified to integrate Var
tokenize :: String -> [String]
tokenize [] = []
tokenize xs @ (x : xs')
    | x `elem` t = [x] : tokenize xs'
    | isDigit x = [y | y <- takeWhile isDigit xs] : (tokenize (dropWhile isDigit xs))
    | isLetter x = [y | y <- takeWhile isLetter xs] : (tokenize (dropWhile isLetter xs))
    | otherwise = tokenize xs'
        where t = ['+', '-', '*']

-- parseExpr recursively goes through list and produces ast
-- Now also integrates Var
parseExpr :: [String] -> (Ast,[String])
parseExpr [] = error "Error!"
parseExpr (s:ss) | all isDigit s = (Tall (read s),ss)
                 | isLetter (head s) = (Var s, ss)
                 | s == "-" = let (e,ss') = parseExpr ss in (Min e,ss')
                 | s == "*" = (Mult e e',ss'')
                 | s == "+" = (Sum e e',ss'') where
                          (e,ss') = parseExpr ss
                          (e',ss'') = parseExpr ss'



-- parse returns the first in the tuple returned from parseExpr
-- Now also integrates Var
parse :: String -> Ast
parse [] = error "Empty string"
parse str = fst $ parseExpr x
  where x = tokenize str



viss :: Ast -> String
viss ast = viss2 ast 0

-- Spacing for viss2
spacing n = concat $ take n (repeat "   ")

-- Helper function for the viss function. Takes in indentation as parameter
-- together with ast. Modified to integrate Var
viss2 :: Ast -> Int -> String
viss2 (Mult x y) n = spacing n ++ "Mult"  ++ "\n" ++ viss2 x (n+1) ++ viss2 y (n+1)
viss2 (Sum  x y) n = spacing n ++ "Sum"   ++ "\n" ++ viss2 x (n+1) ++ viss2 y (n+1)
viss2 (Min  x  ) n = spacing n ++ "Min"   ++ "\n" ++ viss2 x (n+1)
viss2 (Tall x  ) n = spacing n ++ "Tall " ++ show x ++ "\n"
viss2 (Var x   ) n = spacing n ++ "Var " ++ show x ++ "\n"

vis :: Ast -> IO ()

vis ast = putStr (viss ast)



-- Helper function for evi and evb that gives result in a tuple with int
-- and bool
evHelp :: Ast -> [(String, Int)] ->(Int, Bool)
evHelp (Tall n) xs   | even n = (n, False)
                     | otherwise = (n, True)
evHelp (Sum x y) xs  = (fst(evHelp x xs)   + fst(evHelp y xs),
                            snd(evHelp x xs) || snd(evHelp y xs))
evHelp (Min x) xs    = (- fst(evHelp x xs), not(snd(evHelp x xs)))
evHelp (Mult x y) xs = (fst(evHelp x xs)   * fst(evHelp y xs),
                            snd(evHelp x xs) && snd(evHelp y xs))

-- This was my first code for evi. Changed this when i made a combined helper
-- function for both evb and evi
-- evi (Tall x) = x
-- evi (Sum x y) = (evi x) + (evi y)
-- evi (Mult x y) = (evi x) * (evi y)
-- evi (Min x) = 0 - (evi x)

-- Uses helper function evHelp and returns first element of tuple
evi :: Ast -> Int
evi ast = fst(evHelp(ast) [])


-- Uses helper function evHelp and returns second element of tuple
evb :: Ast -> Bool
evb ast = snd(evHelp(ast) [])


--Did not manage to finish evix and evbx
evix :: Ast -> Int -> Int

evix str i = undefined

evbx :: Ast -> Int -> Bool

evbx str i = undefined
