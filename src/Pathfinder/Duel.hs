{-# LANGUAGE MultiWayIf #-}
module Pathfinder.Duel where
    import Probability
    import Probability.Collapse
    import Pathfinder.Internal.Entity
    import Pathfinder.Internal.Defences
    import Pathfinder.Internal.Attack
    import Data.Ratio
    import Control.Monad

    type NextMove = Bool
    left = False
    right = True

    data DuelState = InProgress Entity Entity NextMove
                     | FWon
                     | FLost
                     | BothDead
                     | Unfinished
                     deriving (Ord, Eq, Show)
    

    -- If after the last update it is still in progress,
    -- give up and call it unfinished
    lastRound :: DuelState -> DuelState
    lastRound InProgress {} = Unfinished
    lastRound x             = x

    updateDS :: DuelState -> DuelState
    updateDS (InProgress Dead Dead _) = BothDead
    updateDS (InProgress _ Dead _) = FWon
    updateDS (InProgress Dead _ _) = FLost
    updateDS x = x

    flipDS :: DuelState -> DuelState
    flipDS (InProgress x y n) = InProgress y x (not n)
    flipDS FWon = FLost
    flipDS FLost = FWon
    flipDS x = x

    firstMove :: Entity -> Entity -> Probability Bool
    firstMove x y = normalise $ collapseSort $ do rx <- dice 20
                                                  ry <- dice 20 
                                                  if
                                                    | rx + (initiative . getEnt) x > ry + (initiative . getEnt) y -> return left
                                                    | rx + (initiative . getEnt) x < ry + (initiative . getEnt) y -> return right
                                                    | (initiative . getEnt) x > (initiative . getEnt) y -> return left
                                                    | (initiative . getEnt) x < (initiative . getEnt) y -> return right                                                    | otherwise -> P []

    duel :: Probability Entity -> Probability Entity -> Probability DuelState
    duel x y = collapseSort $ join $ do x' <- x
                                        sDuel x' <$> y


    sDuel :: Entity -> Entity -> Probability DuelState
    sDuel x y = collapseSort $ do init <- firstMove x y
                                  d <- duel' 100 $ InProgress x y init
                                  return $ lastRound d

    duel' :: Int -> DuelState -> Probability DuelState
    duel' 0 xs = return xs
    duel' n xs = collapseSort (duel' (n-1) xs >>= duelRound)
                                

    duelRound :: DuelState -> Probability DuelState
    duelRound s@InProgress {} = collapseSort $ normalise $ do   s' <- duelStep s
                                                                s'' <- duelStep s'
                                                                guard (s'' /= s)
                                                                return s''
    duelRound x = return x

    duelStep :: DuelState -> Probability DuelState
    duelStep (InProgress x y False) = do let attack = (head . getAttack . getEnt) x
                                         s <- attackRoll attack y
                                         y' <- case s of Miss -> return y
                                                         Fumble -> return y -- Do damage to self?
                                                         Hit -> dealDamages y (damage attack) False
                                                         Crit -> dealDamages y (damage attack) True

                                         (return . updateDS) (InProgress x y' right)
    duelStep d@(InProgress _ _ True)  = (fmap flipDS . duelStep . flipDS) d
    duelStep x = return x

    attackRoll :: Attack -> Entity -> Probability AttackResult
    attackRoll a e = collapseSort $ do r <- dice 20
                                       return $ if
                                                 | r ==1 -> Fumble
                                                 | r >= crit a -> Crit
                                                 | r >= (ac . defences .getEnt) e - modifier a -> Hit
                                                 | otherwise -> Miss
