{-# LANGUAGE DeriveGeneric #-}
module Cards where
    import Probability
    import Data.Ratio
    import Data.List
    import Data.Hashable
    import GHC.Generics (Generic)

    data Suit = Clubs | Hearts | Spades | Diamonds deriving(Eq, Show, Ord, Generic)
    data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving(Eq, Show, Ord, Generic)
    instance Hashable Suit
    instance Hashable Value
    suits = [Clubs, Hearts, Spades, Diamonds]
    values = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    
    type Card = (Suit, Value)
    deck :: [Card]
    deck = (,) <$> suits <*> values

    deckN :: Int -> [Card]
    deckN n = concat . replicate n $ deck

    shuffleN :: Int -> Probability [Card]
    shuffleN = return . deckN

    shuffle :: Probability [Card]
    --shuffle :: [Card] -> Probability (Card, [Card])
    shuffle = return deck

    deal  ::[Card] -> Probability (Card, [Card])
    deal deck = do c <- rand deck
                   let d = delete c deck
                   return (c, d)
    
    -- Deals N cards from Deck
    dealN :: [Card] -> Int -> Probability ([Card], [Card])
    dealN deck 0 = return ([], deck)
    dealN deck n = do (c, d) <- deal deck
                      (hand, d') <- dealN d $ n-1
                      return (c:hand, d')
    
    -- Deals N cards to P players
    dealNP :: [Card] -> Int -> Int -> Probability ([[Card]], [Card])
    dealNP deck n 0 = return ([], deck)
    dealNP deck n p = do (h, d) <- dealN deck n
                         (hands, d') <- dealNP d n $ p-1
                         return (h:hands, d')
              