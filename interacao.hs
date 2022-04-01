-- type IO = World -> World
-- type IO a = World -> (a, World)
--
-- Char -> World -> (Int, World)
-- Char -> IO Int

getChar :: IO Char
putChar :: Char -> IO ()
return :: a -> IO a

-- do v1 <- a1
--    v2 <- a2
--    a3
--    .
--    .
--    vn <- an
--    return (f v1 v2 ... vn)

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         z <- getChar
         return (x, z)


getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                return []
              else
                do xs <- getLine'
                   return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Digite uma cadeia de caracteres: "
            xs <- getLine
            putStr "A cadeia tem "
            putStr (show (length xs))
            putStrLn " caracteres."
