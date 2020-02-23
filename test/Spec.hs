import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "dummy" $ do
        it "should show that a test harness is present" $ do
            2+2  `shouldBe` 4
