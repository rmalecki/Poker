module Poker
(getScore,
shuffledDeck)
where

import Data.List (sortBy, nubBy, filter, find)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (catMaybes)
import System.Random (StdGen, randomR)

-- Type definitions --
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Eq, Enum)

instance Show Suit where
  show s = suitChars !! fromEnum s
    where suitChars = ["\x2660", "\x2665", "\x2666", "\x2663"]

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Bounded, Enum)

instance Show Rank where
  show r = rankChars !! fromEnum r
    where rankChars = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]

data HandCategory = HighCard | Pair | TwoPairs | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush
  deriving (Eq, Show, Ord, Bounded, Enum)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq)

instance Show Card where
  show c = show (rank c) ++ show (suit c)

newtype Hand = Hand [Card]
  deriving (Eq, Show)

instance Ord Hand where
  compare a b = compare s1 s2
    where (_,_,s1) = getScore a
          (_,_,s2) = getScore b

-- Utility functions --
sortDescendingRank = sortBy (flip (comparing rank))

findXOfAKind :: Int -> Maybe Rank -> [Card] -> Maybe [Card]
findXOfAKind n excludeRank cards =
  let ranks = reverse $ enumFrom Two
      rs = case excludeRank of
        Just ex -> filter (ex /=) ranks    -- ranks without the one to exclude
        Nothing -> ranks
      cs r = filter ((r == ) . rank) cards  -- all cards of rank r
  in find ((== n) . length) $ map cs rs

-- These functions find winning hands, they all have the type
-- [Card] -> Maybe [Card]

findFlush cards =
  find ((>= 5) . length) $ map (`filterSuit` cards) suits
  where filterSuit s = filter ((== s) . suit)
        suits = [Spades, Hearts, Diamonds, Clubs]

find4OfAKind = findXOfAKind 4 Nothing

find3OfAKind = findXOfAKind 3 Nothing

findPair = findXOfAKind 2 Nothing

findFullHouse cards = do
  trips <- findXOfAKind 3 Nothing cards
  pair <- findXOfAKind 2 Nothing cards
  return $ trips ++ pair

findTwoPairs cards = do
  pair1 <- findXOfAKind 2 Nothing cards
  -- exclude the rank of the first pair when finding the second pair
  pair2 <- findXOfAKind 2 (Just (rank $ head pair1)) cards
  return $ pair1 ++ pair2

findStraight cards =
  let -- throw out duplicate ranks
      uniques = sortDescendingRank $ nubBy ((==) `on` rank) cards
      -- check for straight
      isStraight cs = length cs == 5 && map rank cs == straightFrom (rank $ head cs)
      -- make a descending [Rank] straight starting with r:
      straightFrom r = reverse $ enumFromTo (toEnum $ max 0 (fromEnum r - 4)) r
      go cs
        | isStraight $ take 5 cs = Just cs
        | length cs > 5 = go $ tail cs
        | otherwise = Nothing
  in go uniques

findStraightFlush cards = do
  flush <- findFlush cards
  findStraight flush

-- add highest cards from the rest of the hand as kickers
addKickers hand cards =
  let kickers = filter (`notElem` hand) cards
  in hand ++ take (5 - length hand) kickers

handScore :: (HandCategory, [Card]) -> Int
handScore (h, cs) =
  -- base score for the type of winning hand, plus card scores (kickers)
  fromEnum h * 1000000 + fst rankScores
  where rankScores = foldr (\c (s, m) -> (s + fromEnum (rank c) * m, m * 15)) (0, 1) cs

-- calculates 5-card poker score for a Hand
-- returns (hand category, cards, score)
getScore :: Hand -> (HandCategory, [Card], Int)
getScore (Hand cards) =
  let cSorted = sortDescendingRank cards
      hands = [Just, findPair, findTwoPairs, find3OfAKind, findStraight, findFlush, findFullHouse, find4OfAKind, findStraightFlush]
      -- apply the finding functions, turning the positive results into Just (Hand, [Card])
      go f h = (,) <$> Just h <*> f cSorted
      r = zipWith go hands (enumFrom HighCard)
      -- keep only the Justs, the last one is the highest possible hand
      -- there will always be at least one "HighCard" entry
      (h, cs) = last $ catMaybes r
      -- limit to 5 (in case of 5+ flush) and add kicker cards
      cs' = addKickers (take 5 cs) cSorted
  in
  (h, cs', handScore (h, cs'))


-- return list with the two elements at indices a and b swapped
swap :: Int -> Int -> [a] -> [a]
swap a b xs
  | a == b    = xs
  | b < a     = swap b a xs
  | otherwise = let (s1, s23) = splitAt a xs
                    (x:s2, y:s3) = splitAt (b-a) s23
                in  s1 ++ (y:s2) ++ (x:s3)

-- Knuth Shuffle (Fisher-Yates)
shuffle :: [a] -> StdGen -> [a]
shuffle xs = go (length xs) xs
  where go a xs gen
          | a == 0    = xs
          | otherwise =
            let (b, nextGen) = randomR (0, a) gen
            in go (a-1) (swap (a-1) b xs) nextGen

shuffledDeck :: StdGen -> [Card]
shuffledDeck gen =
  let deck = [Card r s | r <- enumFrom Two, s <- [Spades, Hearts, Diamonds, Clubs]]
  in shuffle deck gen
