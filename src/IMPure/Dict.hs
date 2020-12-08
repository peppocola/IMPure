module IMPure.Dict where

newtype Dict key value = Dict [(key, value)]

instance (Show key, Show value) => Show (Dict key value) where
  show (Dict []) = ""
  show (Dict ((k,v): ps)) = show k ++ "=" ++ show v ++ "\n" ++ show (Dict ps)

--get an empty dictionary
empty :: (Eq key) => Dict key value
empty = Dict []

--check if a dictionary is empty
isempty :: (Eq key) => Dict key value -> Bool
isempty (Dict []) = True
isempty _ = False

--get the value for a given key
get :: (Eq key) => Dict key value -> key -> Maybe value
get (Dict []) _ = Nothing
get (Dict ((k, v) : ps)) key =
  if key == k
    then Just v
    else get (Dict ps) key

--insert into dictionary
insert :: (Eq key) => Dict key value -> key -> value -> Dict key value
insert (Dict []) key value = Dict [(key, value)]
insert (Dict ((k, v) : ps)) key value =
  if key == k
    then Dict ((key, value) : ps)
    else Dict ((k, v) : ds)
  where
    (Dict ds) = insert (Dict ps) key value --unwrap ds from dictionary