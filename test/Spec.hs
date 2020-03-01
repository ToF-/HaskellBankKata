import Test.Hspec
import Bank

userStory01= ["Given a client makes a deposit of 1000.00 on 01/04/2014"
             ,"And a withdrawal of 100.00 on 02/04/2014"
             ,"And a deposit of 500.00 on 10/04/2014"
             ,"When she prints her bank statement"
             ,"Then she would see"
             ,""
             ,"DATE | AMOUNT | BALANCE"
             ,"10/04/2014 | 500.00 | 1400.00"
             ,"02/04/2014 | -100.00 | 900.00"
             ,"01/04/2014 | 1000.00 | 1000.00"]

main :: IO ()
main = hspec $ do
    describe "acceptance test" $ do
        it (unlines userStory01) $ do
            let commands = ["D 1000.00 01/04/2014"
                           ,"W 100.00 02/04/2014"
                           ,"D 500.00 10/04/2014"
                           ,"P"]
            let result = process commands
            result  `shouldBe` unlines ["DATE | AMOUNT | BALANCE"
                                       ,"10/04/2014 | 500.00 | 1400.00"
                                       ,"02/04/2014 | -100.00 | 900.00"
                                       ,"01/04/2014 | 1000.00 | 1000.00"]


    describe "statement line" $ do
        it "should be showable" $ do
            let st = SL (Date 10 04 2014) 500.00 1400.00
            show st `shouldBe` "10/04/2014 | 500.00 | 1400.00"
            let st = SL (Date 02 04 2014) (-100.00) 900.00
            show st `shouldBe` "02/04/2014 | -100.00 | 900.00"


    describe "statement" $ do
        it "should be made from operations" $ do
            let ops = [Deposit (Date 01 04 2014) 1000.0]
            statement ops `shouldBe` [SL (Date 01 04 2014) 1000.00 1000.00]
