-- module LangFeature.Automata ( doNothing) where
module LangFeature.Automata where

-- type Automaton s = (s, s -> s)
data Automaton s = Automaton {
    initial :: s,
    update :: s -> s
    }

-- s0,s1,s2...
listStates :: Automaton s -> [s]
listStates automaton =
    iterate (update automaton) (initial automaton)
    
    -- error "listStates undefined"

doNothing :: Int -> Int
doNothing x = x

