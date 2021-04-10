module BlackJack where
    import Data.List
    import Data.Bifunctor
    import Probability
    import Probability.Collapse
    import Cards
    import BlackJack.Score
    import Control.Monad
    import Control.Applicative
    import Delete

    data BlackJack = TwoCardTrick | FiveCardTrick | Playing [Value] | Bust deriving(Eq, Show, Ord)

    process:: BlackJack -> BlackJack
    process (Playing hand)
         | (length hand ==2) && ((== 21). sum . map cardScore) hand = TwoCardTrick
         | (length hand ==5) && (not . bust) hand = FiveCardTrick
         | bust hand = Bust
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
    score Bust = 0
    score FiveCardTrick = 22


    -- [Player's HandState] [Rest of Deck]
    type BlackJackGame = ([BlackJack], [Card])

    makeGame :: [([Maybe Value], Bool)] -> [Card] -> [Card] -> Probability BlackJackGame
    makeGame hands' knownDiscarded fullDeck = do shortenedDeck <- fromMaybe ( safeDelete' fullDeck knownDiscarded)
                                                 (hands, deck) <- getValidHands hands' shortenedDeck
                                                 guard True -- Check Valid
                                                 return (hands, deck)
    
    getValidHands :: [([Maybe Value], Bool)] -> [Card] -> Probability BlackJackGame
    getValidHands [] deck = return ([], deck)
    getValidHands (h:hs) deck = do (h', d) <- getValidHands' h deck
                                   (hs', d') <- getValidHands hs d
                                   return (h':hs', d')


    getValidHands' :: ([Maybe Value], Bool) -> [Card] -> Probability (BlackJack,[Card])
    getValidHands' (hands', b) deck = do (hand, d) <- getCards hands' deck
                                         let h = (process . Playing) hand
                                         let keep = if b
                                                    then bust hand && (not . bust . init) hand
                                                    else (not . bust) hand
                                         
                                         guard keep
                                         return (h, d)

    getCards :: [Maybe Value] -> [Card] -> Probability ([Value], [Card])
    getCards [] deck = return ([], deck)
    getCards (x:xs) deck = do (u, d) <- getCard x deck
                              (v, d') <- getCards xs d
                              return (u:v, d')


    getCard :: Maybe Value -> [Card] -> Probability (Value, [Card])
    getCard Nothing deck = do (c,d) <- deal deck
                              let v = snd c
                              return (v, d)
    getCard (Just x) deck = return (x, deck)

    twist :: BlackJack -> [Card] -> Probability (BlackJack, [Card])
    twist (Playing x) deck = collapseSort $ do  ((_, c),d) <- deal deck
                                                return ((process . Playing) (c:x), d)
    twist x d = return (x, d)