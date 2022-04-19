module UUID.Dict exposing
    ( Dict
    , empty, insert, update, remove
    , member, get, size
    , keys, fromList
    , foldl
    , toggle
    )

{-| Fork of [elm-core's Dict](/packages/elm/core/1.0.5/Dict), bypassing the "comparable"-key limitation for `UUID`s.
Also includes four extras.

Insert, remove, and query operations all take _O(log n)_ time.


# Dictionaries

@docs Dict


# Build

@docs empty, insert, update, remove


# Query

@docs member, get, size


# Lists

@docs keys, fromList


# Transform

@docs foldl


# Combine


# Extra

@docs toggle

-}

import UUID exposing (UUID)


type NColor
    = Red
    | Black


type Dict v
    = RBNode_elm_builtin NColor UUID v (Dict v) (Dict v)
    | RBEmpty_elm_builtin


empty : Dict v
empty =
    RBEmpty_elm_builtin


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


member : UUID -> Dict v -> Bool
member key dict =
    case get key dict of
        Just _ ->
            True

        Nothing ->
            False


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
        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV (RBNode_elm_builtin Red rlK rlV rlL rlR) rRight) ->
            RBNode_elm_builtin
                Red
                rlK
                rlV
                (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
                (RBNode_elm_builtin Black rK rV rlR rRight)

        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
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
        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
            RBNode_elm_builtin
                Red
                lK
                lV
                (RBNode_elm_builtin Black llK llV llLeft llRight)
                (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

        RBNode_elm_builtin _ k v (RBNode_elm_builtin _ lK lV lLeft lRight) (RBNode_elm_builtin _ rK rV rLeft rRight) ->
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


foldl : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldl func acc dict =
    case dict of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldl func (func key value (foldl func acc left)) right


foldr : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldr func acc t =
    case t of
        RBEmpty_elm_builtin ->
            acc

        RBNode_elm_builtin _ key value left right ->
            foldr func (func key value (foldr func acc right)) left


keys : Dict v -> List UUID
keys dict =
    foldr (\key _ keyList -> key :: keyList) [] dict


fromList : List ( UUID, v ) -> Dict v
fromList assocs =
    List.foldl (\( key, value ) dict -> insert key value dict) empty assocs



-- EXTRA


{-| Like insert, but removes if already exists, discarding the new value too.
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
