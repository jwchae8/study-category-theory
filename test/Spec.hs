import Lib
import Test.Hspec

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
