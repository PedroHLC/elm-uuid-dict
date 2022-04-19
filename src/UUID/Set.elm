module UUID.Set exposing
    ( Set
    , empty, insert, remove
    , member, size
    , toList
    , foldl
    , toggle
    )

{-| Fork of [elm-core's Set](/packages/elm/core/1.0.5/Set), bypassing the "comparable"-key limitation for `UUID`s.
Also includes two extras.

Insert, remove, and query operations all take _O(log n)_ time.


# Sets

@docs Set


# Build

@docs empty, insert, remove


# Query

@docs member, size


# Combine


# Lists

@docs toList


# Transform

@docs foldl


# Extra

@docs toggle

-}

import UUID exposing (UUID)
import UUID.Dict as Dict


type Set
    = Set_uuid (Dict.Dict ())


empty : Set
empty =
    Set_uuid Dict.empty


insert : UUID -> Set -> Set
insert key (Set_uuid dict) =
    Set_uuid (Dict.insert key () dict)


remove : UUID -> Set -> Set
remove key (Set_uuid dict) =
    Set_uuid (Dict.remove key dict)


member : UUID -> Set -> Bool
member key (Set_uuid dict) =
    Dict.member key dict


size : Set -> Int
size (Set_uuid dict) =
    Dict.size dict


toList : Set -> List UUID
toList (Set_uuid dict) =
    Dict.keys dict


foldl : (UUID -> b -> b) -> b -> Set -> b
foldl func initialState (Set_uuid dict) =
    Dict.foldl (\key _ state -> func key state) initialState dict



-- EXTRA


{-| Like insert, but removes if already exists, discarding the new value too.
-}
toggle : UUID -> Set -> Set
toggle key (Set_uuid dict) =
    Set_uuid (Dict.toggle key () dict)
