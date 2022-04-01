-- Tipos I
['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False,'0'),(True,'1')] :: [(Bool, Char)]
([False,True],['0','1']) :: ([Bool], [Char])
[tail,init,reverse] :: [[a] -> [a]]

-- Tipos II
second :: [a] -> a
swap :: (a,b) -> (b,a)
pair :: a -> b -> (a, b)
double :: Num a => a -> a
palindrome :: [a] -> Bool