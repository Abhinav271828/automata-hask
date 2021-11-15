module Regex where

-- Regex Datatype --
data Regex = Phi | Eps | C Char | -- empty regex, characters
             Plus   [Regex] |     -- union
             Concat [Regex] |     -- concatenation
             Star Regex           -- Kleene star
             deriving Show

alph :: [Char] -- the alphabet
alph = ['0','1']

univ :: [Char] -> Regex -- Σ*
univ = (Star . Plus . (map C))

parse :: String -> Regex -- wrapper
parse "" = Phi
parse s = let (r, "") = parse' [] s
          in (reduce r)

parse' :: [Regex] -> String -> ([Regex], String)
-- takes stack, converts string to list of regexes
parse' r        ""       = (r,"")
parse' []       (c:cs)
        | c `elem` alph = parse' [C c] cs
        | c == 'ε'      = parse' [Eps] cs
        | c == '('      = let (r', ')':rest) = parse' [] cs
                          in parse' [(reduce r')] rest
        | otherwise     = parse' [] cs
parse' r@(l:ls) (c:cs)
        | c `elem` alph = parse' ((C c):r) cs
        | c == 'ε'      = parse' (Eps : r) cs
        | c == '*'      = parse' (Star l : ls) cs
        | c == '+'      = let (r', rest) = parse' [] cs
                          in case l of
                               Plus p -> parse' ((Plus $ (reduce r'):p) : ls) rest
                               _      -> parse' ((Plus [reduce r',l]) : ls) rest
        | c == '('      = let (r', ')':rest) = parse' [] cs
                          in parse' ((reduce r'):r) rest
        | c == ')'      = (r, c:cs)
        | otherwise     = parse' r cs

reduce :: [Regex] -> Regex -- generates single regex from stack
reduce [] = Eps
reduce [r] = r
reduce rs = Concat $ reverse rs
