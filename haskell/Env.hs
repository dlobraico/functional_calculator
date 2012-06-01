module Env where

type Key = String
type Env a = [(Key, a)]

empty :: Env a
empty = []

insert :: Key -> a -> Env a -> Env a
insert k v [] = [(k,v)]
insert k v env@((k',v'):env') =
    case compare k k' of
        LT -> (k,v):env
        EQ -> (k,v):env
        GT -> (k',v'):(insert k v env')

bind :: Key -> a -> Env a -> Env a
bind = insert 

lookup :: Key -> Env a -> a
lookup _ [] = error "Unbound variable"
lookup k ((k',v):env) = 
        case compare k k' of
            LT -> error "Unbound variable"
            EQ -> v
            GT -> (Env.lookup k env)
