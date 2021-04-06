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

    shuffleN :: Int -> Probability [Card]
    shuffleN n = return . concat . replicate n $ deck

    shuffle :: Probability [Card]
    --shuffle :: [Card] -> Probability (Card, [Card])
    shuffle = return deck

    deal  ::[Card] -> Probability (Card, [Card])
    deal deck = do c <- rand deck
                   let d = delete c deck
                   return (c, d)
              
              