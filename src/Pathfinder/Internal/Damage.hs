module Pathfinder.Internal.Damage where
    import Probability
    import Pathfinder.Internal.Defences
    
    data DamageType
      = None
      | Slashing
      | Bludgeoning
      | Piercing
      | Fire
      | Cold
      | Electricity
      | Thunder
      | Poison
      | Necrotic
      | Radiant
      | Psychic
      | Force
      | Acid
      | ElementalTraits -- Not sure what this is
      deriving (Show, Eq, Ord)
    
    data DiceValue = DiceValue Int Int Int deriving(Eq, Show, Ord)
    diceValue :: DiceValue -> Probability Int
    diceValue (DiceValue 0 x v) = return v
    diceValue (DiceValue n x v) = do d <- dice x
                                     ds <- diceValue (DiceValue (n-1) x 0)
                                     return (d + ds + v)
    
    data Damage
      = OneOff
          { damageType :: DamageType,
            amount     :: DiceValue
          }
    --  | OngoingDamage
    --      { damageType :: DamageType,
    --        amount :: Dice,
    --        save :: Integer,
    --        skill :: Defence
    --      }
      deriving (Show, Eq, Ord)
    
    data DamageTrait
      = Immune DamageType
      | Resistance DamageType Integer
      | Weakness DamageType
      deriving(Show, Eq, Ord)