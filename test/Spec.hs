import Poker
import System.Random (randomIO, mkStdGen)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "One pair" $ do
    it (show handB ++ " is better than " ++ show handA) $ do
      compare handB handA `shouldBe` GT
  describe "Two pairs" $ do
    it (show handC ++ " is better than " ++ show handB) $ do
      compare handC handB `shouldBe` GT
  describe "Full House" $ do
    it (show handD ++ " is better than " ++ show handC) $ do
      compare handD handC `shouldBe` GT
    it (show handD ++ " is a Full House") $ do
      let (c,_,_) = getScore handD
      c `shouldBe` FullHouse
  where
    handA = Hand [Card Ace Spades, Card Ten Clubs]
    handB = Hand [Card Five Diamonds, Card Five Hearts]
    handC = Hand [Card Six Diamonds, Card Six Hearts, Card Queen Spades, Card Queen Hearts]
    handD = Hand [Card Nine Diamonds, Card Nine Hearts, Card Nine Clubs, Card Queen Spades, Card Queen Hearts]
