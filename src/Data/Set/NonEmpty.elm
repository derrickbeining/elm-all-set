module Data.Set.NonEmpty exposing
    ( NonEmptySet
    , diff
    , filter
    , foldl
    , foldr
    , fromList
    , fromSet
    , insert
    , intersect
    , make
    , map
    , member
    , partition
    , remove
    , singleton
    , size
    , toList
    , toSet
    , union
    )

import Data.Set as Set exposing (Set)


type alias NonEmptySet a =
    ( Set a, a )


{-| Construct a non-empty set from a value and a `Set` of those values
-}
make : a -> Set a -> NonEmptySet a
make a fa =
    ( fa, a )


{-| -}
singleton : a -> NonEmptySet a
singleton a =
    ( Set.empty, a )


{-| -}
insert : a -> NonEmptySet a -> NonEmptySet a
insert a (( init, last ) as fa) =
    if a == last then
        fa

    else
        ( Set.insert last init, a )


remove : a -> NonEmptySet a -> Set a
remove a ( init, last ) =
    if a == last then
        init

    else
        Set.insert last (Set.remove a init)


{-| -}
member : a -> NonEmptySet a -> Bool
member a (( init, last ) as fa) =
    if last == a then
        True

    else
        Set.member a init


{-| -}
size : NonEmptySet a -> Int
size ( init, _ ) =
    Set.size init + 1


{-| -}
union : NonEmptySet a -> NonEmptySet a -> NonEmptySet a
union ( xs, x ) ( ys, y ) =
    if x == y then
        case Set.withdrawLast ys of
            Nothing ->
                ( xs, x )

            Just ( remainingYs, lastY ) ->
                ( Set.union (Set.insert x xs) remainingYs, lastY )

    else
        ( Set.union (Set.insert x xs) ys, y )


{-| -}
intersect : NonEmptySet a -> NonEmptySet a -> Set a
intersect ( xs, x ) ( ys, y ) =
    Set.intersect (Set.insert x xs) (Set.insert y ys)


{-| -}
diff : NonEmptySet a -> NonEmptySet a -> Set a
diff ( xs, x ) ( ys, y ) =
    Set.diff (Set.insert x xs) (Set.insert y ys)


toList : NonEmptySet a -> List a
toList ( init, last ) =
    Set.foldl (::) [ last ] init


{-| -}
fromList : List a -> Maybe (NonEmptySet a)
fromList xs =
    fromSet (Set.fromList xs)


{-| -}
toSet : NonEmptySet a -> Set a
toSet ( init, last ) =
    Set.insert last init


fromSet : Set a -> Maybe (NonEmptySet a)
fromSet =
    Set.withdrawLast


{-| -}
map : (a -> a2) -> NonEmptySet a -> NonEmptySet a2
map f ( fa, a ) =
    ( Set.map f fa, f a )


{-| -}
foldl : (a -> b -> b) -> b -> NonEmptySet a -> b
foldl reduce base ( init, last ) =
    Set.foldl reduce base (Set.insert last init)


{-| -}
foldr : (a -> b -> b) -> b -> NonEmptySet a -> b
foldr reduce base ( init, last ) =
    Set.foldr reduce base (Set.insert last init)


{-| -}
filter : (a -> Bool) -> NonEmptySet a -> Set a
filter test ( init, last ) =
    Set.filter test (Set.insert last init)


{-| -}
partition : (a -> Bool) -> NonEmptySet a -> ( Set a, Set a )
partition test ( init, last ) =
    Set.partition test (Set.insert last init)
