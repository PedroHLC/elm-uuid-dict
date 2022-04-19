module UUID.Set exposing
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    , getFirst, toggle
    )

{-| Fork of [`elm/core`'s `Set`](/packages/elm/core/1.0.5/Set), bypassing the
"comparable"-key limitation for [`UUID`s](/packages/TSFoster/elm-uuid/4.2.0/UUID#UUID).
Also includes some extras.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, singleton, insert, remove


# Query

@docs isEmpty, member, size


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Extra

@docs getFirst, toggle

-}

import Set as Elm
import UUID exposing (UUID)
import UUID.Dict as Dict


{-| Represents a set of unique [`UUID`s](/packages/TSFoster/elm-uuid/4.2.0/UUID#UUID).
-}
type Set
    = Set_uuid (Dict.Dict ())


{-| Create an empty set.
-}
empty : Set
empty =
    Set_uuid Dict.empty


{-| Create a set with one `UUID`.
-}
singleton : UUID -> Set
singleton key =
    Set_uuid (Dict.singleton key ())


{-| Insert a `UUID` into a set.
-}
insert : UUID -> Set -> Set
insert key (Set_uuid dict) =
    Set_uuid (Dict.insert key () dict)


{-| Remove a `UUID` from a set. If the `UUID` is not found, no changes are made.
-}
remove : UUID -> Set -> Set
remove key (Set_uuid dict) =
    Set_uuid (Dict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set -> Bool
isEmpty (Set_uuid dict) =
    Dict.isEmpty dict


{-| Determine if an `UUID` is in a set.
-}
member : UUID -> Set -> Bool
member key (Set_uuid dict) =
    Dict.member key dict


{-| Determine the number of elements in a set.
-}
size : Set -> Int
size (Set_uuid dict) =
    Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : Set -> Set -> Set
union (Set_uuid dict1) (Set_uuid dict2) =
    Set_uuid (Dict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set -> Set -> Set
intersect (Set_uuid dict1) (Set_uuid dict2) =
    Set_uuid (Dict.intersect dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set -> Set -> Set
diff (Set_uuid dict1) (Set_uuid dict2) =
    Set_uuid (Dict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set -> List UUID
toList (Set_uuid dict) =
    Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List UUID -> Set
fromList list =
    List.foldl insert empty list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (UUID -> b -> b) -> b -> Set -> b
foldl func initialState (Set_uuid dict) =
    Dict.foldl (\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (UUID -> b -> b) -> b -> Set -> b
foldr func initialState (Set_uuid dict) =
    Dict.foldr (\key _ state -> func key state) initialState dict


{-| Map a function onto an [`elm/core`'s `Set`](/packages/elm/core/1.0.5/Set),
creating a new set with no duplicates.
-}
map : (UUID -> comparable) -> Set -> Elm.Set comparable
map func set =
    Elm.fromList (foldl (\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

See [`elm/core`'s `Set.filter`](/packages/elm/core/1.0.5/Set#filter) for the original version.

-}
filter : (UUID -> Bool) -> Set -> Set
filter isGood (Set_uuid dict) =
    Set_uuid (Dict.filter (\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (UUID -> Bool) -> Set -> ( Set, Set )
partition isGood (Set_uuid dict) =
    let
        ( dict1, dict2 ) =
            Dict.partition (\key _ -> isGood key) dict
    in
    ( Set_uuid dict1, Set_uuid dict2 )



-- EXTRA


{-| Like insert, but removes if the key already exists int the set, discarding the new value too.
-}
toggle : UUID -> Set -> Set
toggle key (Set_uuid dict) =
    Set_uuid (Dict.toggle key () dict)


{-| Retrieve the node with the smallest key.
-}
getFirst : Set -> Maybe UUID
getFirst (Set_uuid dict) =
    Maybe.map Tuple.first (Dict.getFirst dict)
