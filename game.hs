data State s a =
    State
        { runState :: s -> (a, s)
        }


instance Functor (State s) where

    fmap f (State next) = State next1

        where

        next1 s =
            let (a, s1) = next s
             in (f a, s1)

instance Applicative (State s) where
    pure a = State (\s -> (a, s))

    (<*>) (State nextF) (State nextA) = State nextB

        where

        nextB s =
            let (f, s1) = nextF s
                (a, s2) = nextA s1
             in (f a, s2)

instance Monad (State s) where
    return = pure
    (>>=) (State nextA) newState = State nextBNew
        where
        nextBNew s =
            let (a, s1) = nextA s
                (State nextB) = newState a
             in nextB s1

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do
    s <- get
    put (f s)


data GameState = GameState Bool Int

data GameInput
    = TogglePower -- Toggles the game from on to off and vice-versa
    | Punch       -- You punch, the score increases by 1
    | GetPunched  -- You get punched, the score decreases by 1

type Game a = State GameState a

punch :: Game ()
punch = do
    (GameState isOn score) <- get
    if isOn
    then put (GameState True (score + 1))
    else put (GameState False score)

getPunched :: Game ()
getPunched = do
    (GameState isOn score) <- get
    if isOn
    then put (GameState True (score - 1))
    else put (GameState False score)

togglePower :: Game ()
togglePower = do
    (GameState isOn score) <- get
    put (GameState (not isOn) score)

getScore ::  Game Int
getScore = do
    (GameState _ score) <- get
    return score

toGameCombinator :: GameInput -> Game ()
toGameCombinator Punch = punch
toGameCombinator GetPunched = getPunched
toGameCombinator TogglePower = togglePower

makeGameComputation :: [GameInput] -> Game Int
makeGameComputation [] = getScore
makeGameComputation (x:xs) = toGameCombinator x >> makeGameComputation xs

runGame :: [GameInput] -> Int
runGame input =
    let (score, _) = runState (makeGameComputation input) (GameState False 0)
     in score
