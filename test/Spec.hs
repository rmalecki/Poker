import Poker
import System.Random (randomIO, mkStdGen)

main :: IO ()
main = do
  deck <- shuffledDeck .mkStdGen <$> randomIO
  print deck
  return ()
