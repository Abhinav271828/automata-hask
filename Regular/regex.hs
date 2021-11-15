-- Regex Datatype --
data Regex = Eps  | C Char  |
             Plus   [Regex] |
             Concat [Regex] |
             Star Regex
             deriving Show

alph :: [Char]
alph = ['0','1']

univ :: [Char] -> Regex
univ = (Star . Plus . (map C))

parse :: String -> Regex
parse s = let (r, "") = parse' [] s
          in (reduce r)

parse' :: [Regex] -> String -> ([Regex], String)
parse' r        ""       = (r,"")
parse' []       (c:cs)
        | c `elem` alph = parse' [C c] cs
        | c == '('      = let (r', ')':rest) = parse' [] cs
                          in parse' [(reduce r')] rest
        | otherwise     = parse' [] cs
parse' r@(l:ls) (c:cs)
        | c `elem` alph = parse' ((C c):r) cs
        | c == '*'      = parse' (Star l : ls) cs
        | c == '+'      = let (r', rest) = parse' [] cs
                          in case l of
                               Plus p -> parse' (((Plus $ (reduce r'):p)) : ls) rest
                               _      -> parse' ((Plus [reduce r',l]) : ls) rest
        | c == '('      = let (r', ')':rest) = parse' [] cs
                          in parse' ((reduce r'):r) rest
        | c == ')'      = (r, c:cs)
        | otherwise     = parse' r cs

reduce :: [Regex] -> Regex
reduce [] = Eps
reduce [r] = r
reduce rs = Concat $ reverse rs
