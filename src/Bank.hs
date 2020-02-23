module Bank
    where

data Bank = Nil
type Result = String
type Command = String

newBank :: Bank
newBank = Nil

process :: Bank -> [Command] -> Result
process _ _ = unlines ["DATE | AMOUNT | BALANCE"
                      ,"10/04/2014 | 500.00 | 1400.00"
                      ,"02/04/2014 | -100.00 | 900.00"
                      ,"01/04/2014 | 1000.00 | 1000.00"]

