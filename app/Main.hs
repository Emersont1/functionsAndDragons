module Main where

import Probability
import Probability.Collapse
import Control.Monad
import Pathfinder.Duel
import Pathfinder.Creatures.Wolf
import Pathfinder.Creatures.SmallLightningElemental


duelResult = duel wolf wolf

main :: IO ()
main = print duelResult
