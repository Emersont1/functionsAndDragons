module Pathfinder.Creatures.Wolf (wolf, averageWolf) where

import Probability
import Pathfinder.Internal
import Dice

wolfInternal :: Int -> Entity
wolfInternal e =
  Conscious $
    Entity
      { entityName = "Wolf",
        hp = e,
        maxHP = e,
        defences =
          Defences
            { ac = 14,
              fortitude = 5,
              reflex = 5,
              will = 1
            },
        initiative = 2,
        getAttack =
          [ Attack
              { attackName = "Bite",
                modifier = 2,
                damage = [OneOff None $ DiceValue 1 6 1 ],
                crit = 20,
                critMultiplier = 2
              }
          ],
        damageTraits = []
      }

wolf = fmap wolfInternal $ 2 `d` 8 `p` 4

averageWolf :: Probability Entity
averageWolf = return $ wolfInternal 13
