module Main where
import BlackJack
import Cards
import Probability.Collapse
import Probability


game :: Probability BlackJackGame
game = makeGame [([Just (Four), Just (Seven)], False), ([Just (Six), Nothing], False)] [(Hearts, Five), (Spades, Ace)] (deckN 1)

moves :: Probability Bool
moves = do (players, d) <- game
           let p = head players
           let ps = tail players
           (p', d') <- twist p d
           let dealer = players !! 1
           return $ (score p' > score dealer) && (score p <= score dealer)



main :: IO ()
main = (print . collapseSort) moves
