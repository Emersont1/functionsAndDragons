module Pathfinder.Internal.Attack where
import Pathfinder.Internal.Damage

data Attack = Attack
  { attackName     :: String,
    modifier       :: Int,
    damage         :: [Damage],
    -- Roll value (without mods) that if higher its a crit.
    -- Example: 19-20x2 would be crit = 19, critMultiplier = 2
    crit           :: Int,
    -- Only applies to the first damage
    critMultiplier :: Int
  }
  deriving (Show, Eq, Ord)

data AttackResult = Hit | Miss | Crit | Fumble deriving(Show, Eq, Ord)