module BlackJack where
    import Data.List
    import Probability
    import Probability.Collapse
    import Cards
    import BlackJack.Score
    import Control.Monad
    import Control.Applicative
    import Delete

    data BlackJack = TwoCardTrick | FiveCardTrick | Playing [Value] | Bust [Value] deriving(Eq, Show, Ord)

    process:: BlackJack -> BlackJack
    process (Playing hand)
         | (length hand ==2) && ((== 21). sum . map cardScore) hand = TwoCardTrick
         | (length hand ==5) && (not . bust) hand = FiveCardTrick
         | bust hand = Bust hand
         | otherwise = Playing hand
    process x = x

    bScore :: [Value] -> Int
    hScore :: [Value] -> Int
    bScore = sum . map bustScore
    hScore = sum . map cardScore

    bust :: [Value] -> Bool
    bust = (> 21) . bScore
    
    score :: BlackJack -> Int
    score TwoCardTrick = 21
    score (Playing x)
        | high <= 21 = high
        | otherwise = bScore x
            where high = hScore x
    score (Bust _) = 0
    score FiveCardTrick = 22


    -- [Player's HandState] [Rest of Deck]
    type BlackJackGame = ([BlackJack], [Card])

    makeGame :: [([Maybe Card], Bool)] -> [Card] -> [Card] -> Probability BlackJackGame
    makeGame hands' knownDiscarded fullDeck = do shortenedDeck <- fromMaybe ( safeDelete' fullDeck knownDiscarded)
                                                 (hands, deck) <- getValidHands hands' shortenedDeck
                                                 guard True -- Check Valid
                                                 return (hands, deck)
    
    getValidHands :: [([Maybe Card], Bool)] -> [Card] -> Probability BlackJackGame
    getValidHands [] deck = return ([], deck)
    getValidHands (h:hs) deck = do (h', d) <- getValidHands' h deck
                                   (hs', d') <- getValidHands hs d
                                   return (h':hs', d')


    getValidHands' :: ([Maybe Card], Bool) -> [Card] -> Probability (BlackJack,[Card])
    getValidHands' (hands', b) deck = do (hand, d) <- getCards hands' deck
                                         let h' = map snd hand
                                         let h = (process . Playing) h'
                                         let keep = if b
                                                    then bust h' && (not . bust . init) h' 
                                                    else (not . bust) h'
                                         
                                         guard keep
                                         return (h, d)

    getCards :: [Maybe Card] -> [Card] -> Probability ([Card], [Card])
    getCards [] deck = return ([], deck)
    getCards (x:xs) deck = do (u, d) <- getCard x deck
                              (v, d') <- getCards xs d
                              return (u:v, d')


    getCard :: Maybe Card -> [Card] -> Probability (Card, [Card])
    getCard Nothing deck = deal deck
    getCard (Just x) deck = return (x, deck)

