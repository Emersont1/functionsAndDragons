module Pathfinder.Internal.Entity where
import Probability
import Pathfinder.Internal.Attack
import Pathfinder.Internal.Damage
import Pathfinder.Internal.Defences


data Ent = Entity
  { entityName   :: String,
    hp           :: Int,
    maxHP        :: Int,
    defences     :: Defences,
    initiative   :: Int,
    getAttack    :: [Attack],
    damageTraits :: [DamageTrait]
  }
  deriving (Show, Eq, Ord)

data Entity = Dead | Conscious Ent | Unconscious Ent deriving (Show, Eq, Ord)

extendedRest :: Entity -> Entity
--extendedRest (Dead e)        = Dead e
extendedRest (Conscious e)   = Conscious $ e {hp = maxHP e}
extendedRest (Unconscious e) = Conscious $ e {hp = maxHP e}

getEnt :: Entity-> Ent
--getEnt (Dead a)        = a
getEnt (Conscious a)   = a
getEnt (Unconscious a) = a


update :: Ent -> Entity
update x
  | hp x > 0 = Conscious x
  | False =  Unconscious x
  | otherwise = Dead
-- hp x + constitution ( stats x) > 0 = Unconsious x

-- TODO: Add resistances
dealDamage :: Entity -> Int -> Entity
dealDamage Dead _          = Dead
dealDamage (Unconscious x) dam = update x {hp = hp x - dam }
dealDamage (Conscious x) dam = update x {hp = hp x - dam }

dealDamages :: Entity -> [Damage] -> Bool -> Probability Entity
dealDamages x [] _ = return x;
dealDamages x (d:ds) double = do e <- dealDamages x ds False
                                 dam <- (diceValue . amount) d
                                 let dam' = if double then 2*dam else dam
                                 return $ dealDamage e dam'