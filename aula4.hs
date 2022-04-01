-- exercício a
-- sum [x] -- aplicando sum
-- x + sum [] -- aplicando sum
-- x + 0 -- aplicando +
-- x

-- exercício b
produtorio :: Num a => [a] -> a
produtorio []     = 1
produtorio (n:ns) = n * produtorio ns

-- exercício c
qsortReverso :: Ord a => [a] -> [a]
qsortReverso []     = []
qsortReverso (x:xs) = qsortReverso maiores ++ [x] ++ qsortReverso menores
        where maiores = [a | a <- xs, a >= x ]
              menores = [a | a <- xs, a < x ]

-- exercício d
qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' menores ++ [x] ++ qsort' maiores
        where menores = [a | a <- xs, a < x]
              maiores = [a | a <- xs, a > x]
qsort' [2,2,3,1,1] -- aplicando qsort' 
-- [1,2,3] -- resulta nesta lista
-- como não há uma padrão para os elementos iguais ao pivot, os mesmos
-- descartados

-- exercício e
-- qsort [3,5,1,4,2]
-- qsort [1,2] ++ [3] ++ qsort [5,4]
-- (qsort [] ++ [1] ++ qsort [2]) ++ [3] ++ (qsort [4] ++ [5] ++ qsort [])
-- ([] ++ [1] ++ (qsort [] ++ [2] ++ qsort [])) ++ [3] ++ ((qsort [] ++ [4] ++ qsort []) ++ [5] ++ [])
-- ([] ++ [1] ++ ([] ++ [2] ++ [])) ++ [3] ++ (([] ++ [4] ++ []) ++ [5] ++ [])
-- ([] ++ [1] ++ [2] ) ++ [3] ++ ([4] ++ [5] ++ [])
-- ([1] ++ [2] ) ++ [3] ++ ([4] ++ [5])
-- [1,2] ++ [3] ++ [4,5]
-- [1,2,3,4,5]

-- exercício f
-- qsort "afd345"
-- "345adf"
