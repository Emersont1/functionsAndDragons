module BlackJack.Score where
    import Cards

    bustScore :: Value -> Int
    bustScore Ace = 1
    bustScore Two = 2
    bustScore Three = 3
    bustScore Four = 4
    bustScore Five = 5
    bustScore Six = 6
    bustScore Seven = 7
    bustScore Eight = 8
    bustScore Nine = 9
    bustScore _ = 10

    cardScore :: Value -> Int
    cardScore Ace = 11
    cardScore x = bustScore x

