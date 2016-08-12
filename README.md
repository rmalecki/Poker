This is a tiny module to rank Poker hands which I whipped up as a Haskell exercise.

Use `shuffledDeck` with a StdGen to create a Knuth-shuffled deck:
```haskell
λ: let deck = shuffledDeck $ mkStdGen 4
λ: deck
[K♦,2♦,7♠,K♠,10♣,K♥,9♥,6♠,10♥,10♠,5♣,J♠,5♥,A♥,A♦,2♣,6♥,2♠,A♠,K♣,9♦,8♥,3♣,Q♠,8♣,J♦,J♣,4♠,A♣,J♥,6♦,10♦,8♦,2♥,3♥,Q♣,9♣,4♥,8♠,Q♦,9♠,7♦,7♥,3♠,4♦,7♣,6♣,4♣,Q♥,5♦,5♠,3♦]
```

Set up a game of Texas Hold'em:

```haskell
λ: let board = take 5 deck
λ: board
[K♦,2♦,7♠,K♠,10♣]
λ: let handA = Hand $ board ++ (take 2 . drop 5) deck
λ: let handB = Hand $ board ++ (take 2 . drop 7) deck
λ: handA
Hand [K♦,2♦,7♠,K♠,10♣,K♥,9♥]
λ: handB
Hand [K♦,2♦,7♠,K♠,10♣,6♠,10♥]
```
TODO: Put the deck into a State monad that updates properly when dealing cards.

Use `getScore` to return the type of winning hand, the five cards for that hand and a score value:
```haskell
λ: getScore handA
(ThreeOfAKind,[K♦,K♠,K♥,10♣,9♥],3596602)
λ: getScore handB
(TwoPairs,[K♦,K♠,10♣,10♥,7♠],2595925)
```

You can can also compare two Hands directly
```haskell
λ: compare handA handB
GT
```
