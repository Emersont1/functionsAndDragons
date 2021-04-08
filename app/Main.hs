module Main where
import BlackJack
import Cards
import Probability.Collapse
import Probability


game :: Probability BlackJackGame
game = makeGame [([Just (Spades, Ten), Just (Clubs, King)], False), ([Just (Hearts, Four), Nothing], False)] [(Hearts, Five), (Spades, Ace)] (deckN 1)

main :: IO ()
main = (print . collapseSort . fmap fst) game
