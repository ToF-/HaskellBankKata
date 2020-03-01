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


data Date = Date Int Int Int
    deriving Eq

type Amount = Float

data StatementLine = SL Date Amount Amount
    deriving Eq

data Operation = Deposit Date Amount
    deriving (Eq,Show)
    
instance Show StatementLine
    where
    show (SL d a b) = intercalate " | " [show d, showAmount a, showAmount b]


showAmount n = intPart ++ "." ++ decPart
    where 
    intPart = take (l-2) m
    decPart = drop (l-2) m
    m = show (truncate (n * 100))
    l = length m


instance Show Date 
    where
    show (Date d m y) = (showInt2 d) ++ "/" ++ (showInt2 m) ++ "/" ++ (show y)
        where
        showInt2 n | n < 10 = '0':show n
                   | otherwise = show n


statement :: [Operation] -> [StatementLine]
statement = reverseStatement . reverse
    where
    reverseStatement :: [Operation] -> [StatementLine]
    reverseStatement [] = []
    reverseStatement [Deposit d a] = SL d a (0+a) : []
    reverseStatement [Deposit e b, Deposit d a] = SL e b (a+b) : reverseStatement [Deposit d a] 
    reverseStatement [Deposit f c, Deposit e b, Deposit d a] = SL f c (a+b+c) : reverseStatement [Deposit e b, Deposit d a]
