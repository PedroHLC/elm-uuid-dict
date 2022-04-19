module UUID.Dict exposing
    ( Dict
    , empty, singleton, insert, update, remove
    , isEmpty, member, get, size
    , keys, values, toList, fromList
    , map, foldl, foldr, filter, partition
    , union, intersect, diff, merge
    , getFirst, findl, findr, toggle
    )

{-| Fork of [`elm/core`'s `Dict`](/packages/elm/core/1.0.5/Dict), bypassing the
"comparable"-key limitation for [`UUID`s](/packages/TSFoster/elm-uuid/4.2.0/UUID#UUID).

Also includes some extras.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs Dict


# Build

@docs empty, singleton, insert, update, remove


# Query

@docs isEmpty, member, get, size


# Lists

@docs keys, values, toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition


# Combine

@docs union, intersect, diff, merge


# Extra

@docs getFirst, findl, findr, toggle

-}

import UUID exposing (UUID)


type NColor
    = Red
    | Black


{-| A dictionary of [`UUID`s](/packages/TSFoster/elm-uuid/4.2.0/UUID#UUID) and values.

See [`elm/core`'s `Dict.Dict`](/packages/elm/core/1.0.5/Dict#Dict) for the original version.

    import UUID
    import UUID.Dict

    initUsers : Seeds -> List User -> ( Seeds, UUID.Dict User )
    initUsers seed1 =
        let
            ( id1, seed2 ) =
                UUID.step seed1

            ( id2, seed3 ) =
                UUID.step seed2

            ( id3, seed4 ) =
                UUID.step seed2
        in
        UUID.Dict.fromList
            [ ( id1, User "Alice" 28 1.65 )
            , ( id2, User "Bob" 19 1.82 )
            , ( id3, User "Chuck" 33 1.75 )
            ]
            |> Tuple.pair seed4

    type alias User =
        { name : String
        , age : Int
        , height : Float
        }

-}
type Dict v
    = RBNode_elm_builtin NColor UUID v (Dict v) (Dict v)
    | RBEmpty_elm_builtin


{-| Create an empty dictionary.
-}
empty : Dict v
empty =
    RBEmpty_elm_builtin


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

See [`elm/core`'s `Dict.get`](/packages/elm/core/1.0.5/Dict#get) for the original version.

-}
get : UUID -> Dict v -> Maybe v
get targetKey dict =
    case dict of
        RBEmpty_elm_builtin ->
            Nothing

        RBNode_elm_builtin _ key value left right ->
            case UUID.compare targetKey key of
                LT ->
                    get targetKey left

                EQ ->
                    Just value

                GT ->
                    get targetKey right


{-| Determine if a key is in a dictionary.
-}
member : UUID -> Dict v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


{-| Determine the number of key-value pairs in the dictionary.
-}
size : Dict v -> Int
size dict =
    sizeHelp 0 dict


sizeHelp : Int -> Dict v -> Int
sizeHelp n dict =
    case dict of
        RBEmpty_elm_builtin ->
            n

        RBNode_elm_builtin _ _ _ left right ->
            sizeHelp (sizeHelp (n + 1) right) left


{-| Determine if a dictionary is empty.
isEmpty empty == True
-}
isEmpty : Dict v -> Bool
isEmpty dict =
    case dict of
        RBEmpty_elm_builtin ->
            True

        RBNode_elm_builtin _ _ _ _ _ ->
            False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision.
-}
insert : UUID -> v -> Dict v -> Dict v
insert key value dict =
    case insertHelp key value dict of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


insertHelp : UUID -> v -> Dict v -> Dict v
insertHelp key value dict =
    case dict of
        RBEmpty_elm_builtin ->
            RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

        RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
            case UUID.compare key nKey of
                LT ->
                    balance nColor nKey nValue (insertHelp key value nLeft) nRight

                EQ ->
                    RBNode_elm_builtin nColor nKey value nLeft nRight

                GT ->
                    balance nColor nKey nValue nLeft (insertHelp key value nRight)


balance : NColor -> UUID -> v -> Dict v -> Dict v -> Dict v
balance color key value left right =
    case right of
        RBNode_elm_builtin Red rK rV rLeft rRight ->
            case left of
                RBNode_elm_builtin Red lK lV lLeft lRight ->
                    RBNode_elm_builtin
                        Red
                        key
                        value
                        (RBNode_elm_builtin Black lK lV lLeft lRight)
                        (RBNode_elm_builtin Black rK rV rLeft rRight)

                _ ->
                    RBNode_elm_builtin color rK rV (RBNode_elm_builtin Red key value left rLeft) rRight

        _ ->
            case left of
                RBNode_elm_builtin Red lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight ->
                    RBNode_elm_builtin
                        Red
                        lK
                        lV
                        (RBNode_elm_builtin Black llK llV llLeft llRight)
                        (RBNode_elm_builtin Black key value lRight right)

                _ ->
                    RBNode_elm_builtin color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made.
-}
remove : UUID -> Dict v -> Dict v
remove key dict =
    case removeHelp key dict of
        RBNode_elm_builtin Red k v l r ->
            RBNode_elm_builtin Black k v l r

        x ->
            x


removeHelp : UUID -> Dict v -> Dict v
removeHelp targetKey dict =
    case dict of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            if UUID.compare targetKey key == LT then
                case left of
                    RBNode_elm_builtin Black _ _ lLeft _ ->
                        case lLeft of
                            RBNode_elm_builtin Red _ _ _ _ ->
                                RBNode_elm_builtin color key value (removeHelp targetKey left) right

                            _ ->
                                case moveRedLeft dict of
                                    RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                        balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                                    RBEmpty_elm_builtin ->
                                        RBEmpty_elm_builtin

                    _ ->
                        RBNode_elm_builtin color key value (removeHelp targetKey left) right

            else
                removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : UUID -> Dict v -> NColor -> UUID -> v -> Dict v -> Dict v -> Dict v
removeHelpPrepEQGT targetKey dict color key value left right =
    case left of
        RBNode_elm_builtin Red lK lV lLeft lRight ->
            RBNode_elm_builtin
                color
                lK
                lV
                lLeft
                (RBNode_elm_builtin Red key value lRight right)

        _ ->
            case right of
                RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _ ->
                    moveRedRight dict

                RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
                    moveRedRight dict

                _ ->
                    dict


removeHelpEQGT : UUID -> Dict v -> Dict v
removeHelpEQGT targetKey dict =
    case dict of
        RBNode_elm_builtin color key value left right ->
            if targetKey == key then
                case getMin right of
                    RBNode_elm_builtin _ minKey minValue _ _ ->
                        balance color minKey minValue left (removeMin right)

                    RBEmpty_elm_builtin ->
                        RBEmpty_elm_builtin

            else
                balance color key value left (removeHelp targetKey right)

        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin


getMin : Dict v -> Dict v
getMin dict =
    case dict of
        RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
            getMin left

        _ ->
            dict


removeMin : Dict v -> Dict v
removeMin dict =
    case dict of
        RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
            case lColor of
                Black ->
                    case lLeft of
                        RBNode_elm_builtin Red _ _ _ _ ->
                            RBNode_elm_builtin color key value (removeMin left) right

                        _ ->
                            case moveRedLeft dict of
                                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                                    balance nColor nKey nValue (removeMin nLeft) nRight

                                RBEmpty_elm_builtin ->
                                    RBEmpty_elm_builtin

                _ ->
                    RBNode_elm_builtin color key value (removeMin left) right

        _ ->
            RBEmpty_elm_builtin


moveRedLeft : Dict v -> Dict v
moveRedLeft dict =
    case dict of
        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV ((RBNode_elm_builtin Red rlK rlV rlL rlR) as rLeft) rRight) ->
            RBNode_elm_builtin
                Red
                rlK
                rlV
                (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
                (RBNode_elm_builtin Black rK rV rlR rRight)

        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            dict


moveRedRight : Dict v -> Dict v
moveRedRight dict =
    case dict of
        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            RBNode_elm_builtin
                Red
                lK
                lV
                (RBNode_elm_builtin Black llK llV llLeft llRight)
                (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

        RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
            case clr of
                Black ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

                Red ->
                    RBNode_elm_builtin
                        Black
                        k
                        v
                        (RBNode_elm_builtin Red lK lV lLeft lRight)
                        (RBNode_elm_builtin Red rK rV rLeft rRight)

        _ ->
            dict


update : UUID -> (Maybe v -> Maybe v) -> Dict v -> Dict v
update targetKey alter dictionary =
    case alter (get targetKey dictionary) of
        Just value ->
            insert targetKey value dictionary

        Nothing ->
            remove targetKey dictionary


singleton : UUID -> v -> Dict v
singleton key value =
    RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict v -> Dict v -> Dict v
union t1 t2 =
    foldl insert t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict v -> Dict v -> Dict v
intersect t1 t2 =
    filter (\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict a -> Dict b -> Dict a
diff t1 t2 =
    foldl (\k v t -> remove k t) t1 t2


{-| Combines two dictionaries.

See [`elm/core`'s `Dict.merge`](/packages/elm/core/1.0.5/Dict#merge) for the original version.

-}
merge :
    (UUID -> a -> result -> result)
    -> (UUID -> a -> b -> result -> result)
    -> (UUID -> b -> result -> result)
    -> Dict a
    -> Dict b
    -> result
    -> result
merge leftStep bothStep rightStep leftDict rightDict initialResult =
    let
        stepState rKey rValue ( list, result ) =
            case list of
                [] ->
                    ( list, rightStep rKey rValue result )

                ( lKey, lValue ) :: rest ->
                    case UUID.compare lKey rKey of
                        LT ->
                            stepState rKey rValue ( rest, leftStep lKey lValue result )

                        GT ->
                            ( list, rightStep rKey rValue result )

                        EQ ->
                            ( rest, bothStep lKey lValue rValue result )

        ( leftovers, intermediateResult ) =
            foldl stepState ( toList leftDict, initialResult ) rightDict
    in
    List.foldl (\( k, v ) result -> leftStep k v result) intermediateResult leftovers


{-| Apply a function to all values in a dictionary.
-}
map : (UUID -> a -> b) -> Dict a -> Dict b
map func dict =
    case dict of
        RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin

        RBNode_elm_builtin color key value left right ->
            RBNode_elm_builtin color key (func key value) (map func left) (map func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

See [`elm/core`'s `Dict.foldl`](/packages/elm/core/1.0.5/Dict#foldl) for the original version.

-}
foldl : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldl func acc dict =
    case dict of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl func (func key value (foldl func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

See [`elm/core`'s `Dict.foldr`](/packages/elm/core/1.0.5/Dict#foldr) for the original version.

-}
foldr : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldr func acc t =
    case t of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr func (func key value (foldr func acc right)) left


{-| Keep only the key-value pairs that pass the given test.
-}
filter : (UUID -> v -> Bool) -> Dict v -> Dict v
filter isGood dict =
    foldl
        (\k v d ->
            if isGood k v then
                insert k v d

            else
                d
        )
        empty
        dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (UUID -> v -> Bool) -> Dict v -> ( Dict v, Dict v )
partition isGood dict =
    let
        add key value ( t1, t2 ) =
            if isGood key value then
                ( insert key value t1, t2 )

            else
                ( t1, insert key value t2 )
    in
    foldl add ( empty, empty ) dict


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

See [`elm/core`'s `Dict.keys`](/packages/elm/core/1.0.5/Dict#keys) for the original version.

-}
keys : Dict v -> List UUID
keys dict =
    foldr (\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

See [`elm/core`'s `Dict.values`](/packages/elm/core/1.0.5/Dict#values) for the original version.

-}
values : Dict v -> List v
values dict =
    foldr (\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys.
-}
toList : Dict v -> List ( UUID, v )
toList dict =
    foldr (\key value list -> ( key, value ) :: list) [] dict


{-| Convert an association list into a dictionary.
-}
fromList : List ( UUID, v ) -> Dict v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs



-- EXTRA


{-| Like insert, but removes if the key already exists in the dictionary, discarding the new value too.
-}
toggle : UUID -> v -> Dict v -> Dict v
toggle key value dict =
    update key
        (\past ->
            case past of
                Just _ ->
                    Nothing

                Nothing ->
                    Just value
        )
        dict


{-| Retrieve the first node that matches a predicate.
-}
findl : (UUID -> v -> Bool) -> Dict v -> Maybe ( UUID, v )
findl predicate dict =
    let
        find_ dict_ =
            case dict_ of
                RBEmpty_elm_builtin ->
                    Nothing

                RBNode_elm_builtin _ key value left right ->
                    if predicate key value then
                        Just ( key, value )

                    else
                        case find_ left of
                            Nothing ->
                                find_ right

                            found ->
                                found
    in
    find_ dict


{-| Retrieve the last node that matches a predicate.
-}
findr : (UUID -> v -> Bool) -> Dict v -> Maybe ( UUID, v )
findr predicate dict =
    let
        find_ dict_ =
            case dict_ of
                RBEmpty_elm_builtin ->
                    Nothing

                RBNode_elm_builtin _ key value left right ->
                    if predicate key value then
                        Just ( key, value )

                    else
                        case find_ right of
                            Nothing ->
                                find_ left

                            found ->
                                found
    in
    find_ dict


{-| Retrieve the node with the smallest key.
-}
getFirst : Dict v -> Maybe ( UUID, v )
getFirst dict =
    case getMin dict of
        RBNode_elm_builtin _ minKey minValue _ _ ->
            Just ( minKey, minValue )

        RBEmpty_elm_builtin ->
            Nothing
