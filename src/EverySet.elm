module EverySet exposing
    ( EverySet
    , empty, singleton, insert, remove
    , equals, isEmpty, member, size, withdrawFirst, withdrawLast
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition
    )

{-| A set of unique values. The values can be any type, as the implementation is
based on [AssocList](https://package.elm-lang.org/packages/pzp1997/assoc-list/latest)


# Sets

@docs EverySet


# Build

@docs empty, singleton, insert, remove


# Query

@docs equals, isEmpty, member, size, withdrawFirst, withdrawLast


# Combine

@docs union, intersect, diff


# Lists

@docs toList, fromList


# Transform

@docs map, foldl, foldr, filter, partition

-}

import AssocList exposing (Dict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type EverySet a
    = EverySet (Dict a ())


{-| Create an empty set.
-}
empty : EverySet a
empty =
    EverySet AssocList.empty


{-| Compare two sets for equality, ignoring insertion order.
Sets are defined to be equal when they have identical elements
where elements are compared using the built-in equality operator.

**You should almost never use the built-in equality operator to compare
sets from this module since association lists have no canonical form.**

-}
equals : EverySet a -> EverySet a -> Bool
equals (EverySet dictA) (EverySet dictB) =
    AssocList.eq dictA dictB


{-| Create a set with one value.
-}
singleton : a -> EverySet a
singleton k =
    EverySet <| AssocList.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> EverySet a -> EverySet a
insert k (EverySet d) =
    EverySet <| AssocList.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> EverySet a -> EverySet a
remove k (EverySet d) =
    EverySet <| AssocList.remove k d


{-| Take the last-inserted value of the `Set` and get the `Set` with that
value removed.
-}
withdrawLast : EverySet a -> Maybe ( EverySet a, a )
withdrawLast fa =
    let
        go a result =
            case result of
                Nothing ->
                    Just ( remove a fa, a )

                _ ->
                    result
    in
    foldl go Nothing fa


{-| Take the first-inserted value of the `Set` and get the `Set` with that
value removed.
-}
withdrawFirst : EverySet a -> Maybe ( EverySet a, a )
withdrawFirst fa =
    let
        go a result =
            case result of
                Nothing ->
                    Just ( remove a fa, a )

                _ ->
                    result
    in
    foldr go Nothing fa


{-| Determine if a set is empty.
-}
isEmpty : EverySet a -> Bool
isEmpty (EverySet d) =
    AssocList.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> EverySet a -> Bool
member k (EverySet d) =
    AssocList.member k d


{-| Determine the number of elements in a set.
-}
size : EverySet a -> Int
size (EverySet d) =
    AssocList.size d


{-| Get the union of two sets. Keep all values.
-}
union : EverySet a -> EverySet a -> EverySet a
union (EverySet d1) (EverySet d2) =
    EverySet <| AssocList.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : EverySet a -> EverySet a -> EverySet a
intersect (EverySet d1) (EverySet d2) =
    EverySet <| AssocList.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : EverySet a -> EverySet a -> EverySet a
diff (EverySet d1) (EverySet d2) =
    EverySet <| AssocList.diff d1 d2


{-| Convert a `Set` to a `List`, ordering the elements by the order in which
they were inserted into the `Set`. This property upholds symmetry with
`fromList` so that

    fromList (toList (fromList list)) == fromList list

This is useful because `Set`s are equal (==) only if they have the same
elements AND they were inserted in the same order. So we can go from `Set` to
`List` and back without breaking the structural equality of a given `Set`.

However, `equals` should almost always be used instead of (==) when checking
for `Set` equality, since it is not dependent on insertion order.

-}
toList : EverySet a -> List a
toList =
    foldl (::) []


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> EverySet a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> EverySet a -> b
foldl f b (EverySet d) =
    AssocList.foldl (\k _ result -> f k result) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> EverySet a -> b
foldr f b (EverySet d) =
    AssocList.foldr (\k _ result -> f k result) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a2) -> EverySet a -> EverySet a2
map f s =
    fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> EverySet a -> EverySet a
filter p (EverySet d) =
    EverySet <| AssocList.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> EverySet a -> ( EverySet a, EverySet a )
partition p (EverySet d) =
    let
        ( p1, p2 ) =
            AssocList.partition (\k _ -> p k) d
    in
    ( EverySet p1, EverySet p2 )
