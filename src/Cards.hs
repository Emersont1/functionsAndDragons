module Cards where
    import Probability
    import Data.Ratio
    import Control.Monad.Trans.State.Lazy
    import Control.Monad.Trans.Class

    data Suit = Clubs | Hearts | Spades | Diamonds deriving(Eq, Show)
    data Value = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving(Eq, Show)
    suits = [Clubs, Hearts, Spades, Diamonds]
    values = [Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King]
    
    type Card = (Suit, Value)
    deck :: [Card]
    deck = (,) <$> suits <*> values

    --shuffle :: Probability [Card]
    shuffle :: [Card] -> Probability (Card, [Card])
    shuffle = runStateT deck

    deal  :: StateT [Card] Probability Card
    deal = do shoe <- get
              c <- lift $ rand shoe
              put [y|y<-shoe, y/=c]
              return c
              
              