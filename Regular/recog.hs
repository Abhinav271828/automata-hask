import Regex

recog :: Regex -> String -> Bool
recog r s = elem "" $ recog' r s

recog' :: Regex -> String -> [String]
recog'  Phi             _  = []
recog'  Eps             "" = [""]
recog' (Star    _    )  "" = [""]
recog' (Concat  []   )  cs = [cs]
recog'  _               "" = []
recog' (C       x    ) (c:cs) | c == x
                           = [cs]
                              | otherwise
                           = []
recog' (Plus    rs   )  cs = rs >>= (\r -> recog' r cs)
recog' (Concat (r:rs))  cs = (recog' r cs) >>=
                                recog' (Concat rs)
recog' (Star    r    )  cs = (:) cs $
                               (recog' r cs) >>=
                                  (recog' (Star r))
