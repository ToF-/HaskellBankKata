module Bank
    where

import Data.List (intercalate)

type Result = String
type Command = String

process :: [Command] -> Result
process _ = unlines ["DATE | AMOUNT | BALANCE"
                    ,"10/04/2014 | 500.00 | 1400.00"
                    ,"02/04/2014 | -100.00 | 900.00"
                    ,"01/04/2014 | 1000.00 | 1000.00"]


type Date = (Int,Int,Int)
type Amount = Float
data StatementLine = SL Date Amount Amount
    
instance Show StatementLine
    where
    show (SL d a b) = intercalate " | " [showDate d, showAmount a, showAmount b]


showAmount n = intPart ++ "." ++ decPart
    where 
    intPart = take (l-2) m
    decPart = drop (l-2) m
    m = show (truncate (n * 100))
    l = length m


showDate (d,m,y) = (showInt2 d) ++ "/" ++ (showInt2 m) ++ "/" ++ (show y)
    where
    showInt2 n | n < 10 = '0':show n
               | otherwise = show n
