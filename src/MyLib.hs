module MyLib (takeHeads, nextDiag, diagonals, someFunc) where

import Data.List ()
import Data.Maybe (maybeToList)
import GHC.Stack (whoCreated)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

primesUpto :: Integer -> [Integer]
primesUpto = undefined

headM :: [a] -> Maybe a
headM [] = Nothing
headM (a:_) = Just a 

splitAtM :: [a] -> (Maybe a, [a])
splitAtM [] = (Nothing, [])
splitAtM (a:as) = (Just a, as)

listMtoList :: [Maybe a] -> [a]
listMtoList [] = [] 
listMtoList (Nothing : ls) = listMtoList ls 
listMtoList (Just x : ls) = x : listMtoList ls 

takeHeadsM :: Integer -> [[a]] -> [Maybe a]
takeHeadsM 0 lst = []
takeHeadsM _ [] = []
takeHeadsM n (l:lst) = headM l : takeHeadsM (n - 1) lst 

takeHeads :: Integer -> [[a]] -> [a]
takeHeads n ls = joinAll $ map maybeToList $ takeHeadsM n ls

diagonals2 :: [[a]] -> [[a]]
diagonals2 [] = []
diagonals2 lst = nextDiag' 1 lst
    where 
        nextDiag' n (l:ls) = takeHeads n (l:ls) : nextDiag' (n + 1) (l:ls)

joinAll :: [[a]] -> [a]
joinAll [] = []
joinAll x = undefined

------------------------------------------------------------

tail' :: [a] -> [a]
tail' [] = []
tail' (_:as) = as

--onestep
-- даётся диагональ и таблица над ней, вернуть следующую диагональ и остаток таблицы
nextDiag :: [a] -> [[a]] -> ([a], [[a]])
nextDiag _ [] = ([],[])
nextDiag as ([]:ls) = nextDiag as ls
nextDiag [] (l:ls) = ( listMtoList [headM l], tail l : ls )
nextDiag (_:as) (l:ls) = (lhead ++ diag, tail l : table)
    where
        lhead = listMtoList [headM l]
        (diag, table) = nextDiag as ls



diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals (t:ts) = diagonals' [] (t:ts) []
    where
        diagonals' _ [] acc = reverse acc
        diagonals' ds t acc = diagonals' d' t' (d' : acc)
            where 
                (d', t') = nextDiag ds t

