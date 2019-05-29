import Lib
import Test.Hspec
import Control.Exception (evaluate)
import GHC.Float

inc :: Int -> Int
inc x = x + 1

dec :: Int -> Int
dec x = x - 1

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "Identity function" $ do
      myId 5 `shouldBe` 5
      myId "asdf" `shouldBe` "asdf"
    it "Compose function" $ do
      compose inc dec 5 `shouldBe` 5
      compose inc inc 5 `shouldBe` 7
    it "Composition repects identity" $ do
      fid inc 5 `shouldBe` 6
      idf inc 5 `shouldBe` 6
    it "Bool -> Bool functions" $ do
      boolOne True `shouldBe` True
      boolOne False `shouldBe` False
      boolTwo True `shouldBe` False
      boolTwo False `shouldBe` True
      boolThree True `shouldBe` True
      boolThree True `shouldBe` True
      boolFour True `shouldBe` False
      boolFour False `shouldBe` False
      boolFive True `shouldBe` True
      evaluate(boolFive False) `shouldThrow` anyException
      evaluate(boolSix True) `shouldThrow` anyException
      boolSix False `shouldBe` False
      boolSeven True `shouldBe` False
      evaluate(boolSeven False) `shouldThrow` anyException
      evaluate(boolEight True) `shouldThrow` anyException
      boolEight False `shouldBe` True
      evaluate(boolNine True) `shouldThrow` anyException
      evaluate(boolNine False) `shouldThrow` anyException
    it "Kleisli category for safe number calculation" $ do
      safe_root_reciprocal 4 `shouldBe` (double2Float 0.5, True)
      safe_root_reciprocal 0 `shouldBe` (double2Float 0.0, False)
      safe_root_reciprocal (-1) `shouldBe` (double2Float 0.0, False)
