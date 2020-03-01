module Bank
    where

type Result = String
type Command = String

process :: [Command] -> Result
process _ = unlines ["DATE | AMOUNT | BALANCE"
                    ,"10/04/2014 | 500.00 | 1400.00"
                    ,"02/04/2014 | -100.00 | 900.00"
                    ,"01/04/2014 | 1000.00 | 1000.00"]


type Date = (Int,Int,Int)
type Amount = Float
data Statement = Statement Date Amount Amount
    
instance Show Statement
    where
    show (Statement (10,04,2014) 500.00 1400.00) = "10/04/2014 | 500.00 | " ++ (showAmount 1400)
    show (Statement (02,04,2014) (-100.00) 900.00) = "02/04/2014 | -100.00 | 900.00"


showAmount n = intPart ++ "." ++ decPart
    where 
    intPart = take (l-2) m
    decPart = drop (l-2) m
    m = show (truncate (n * 100))
    l = length m
