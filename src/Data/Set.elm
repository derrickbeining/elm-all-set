module Data.Set exposing
    ( empty, singleton, insert, remove
    , equals, isEmpty, member, size, withdrawFirst, withdrawLast
    , union, intersect, diff
    , toList, fromList
    , map, foldl, foldr, filter, partition, findFirst, findLast
    , Set
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

@docs map, foldl, foldr, filter, partition, findFirst, findLast

-}

import AssocList exposing (Dict)


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set a
    = Set (Dict a ())


{-| Create an empty set.
-}
empty : Set a
empty =
    Set AssocList.empty


{-| Compare two sets for equality, ignoring insertion order.
Sets are defined to be equal when they have identical elements
where elements are compared using the built-in equality operator.

**You should almost never use the built-in equality operator to compare
sets from this module since association lists have no canonical form.**

-}
equals : Set a -> Set a -> Bool
equals (Set dictA) (Set dictB) =
    AssocList.eq dictA dictB


{-| Create a set with one value.
-}
singleton : a -> Set a
singleton k =
    Set <| AssocList.singleton k ()


{-| Insert a value into a set.
-}
insert : a -> Set a -> Set a
insert k (Set d) =
    Set <| AssocList.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : a -> Set a -> Set a
remove k (Set d) =
    Set <| AssocList.remove k d


{-| Take the last-inserted value of the `Set` and get the `Set` with that
value removed.
-}
withdrawLast : Set a -> Maybe ( Set a, a )
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
withdrawFirst : Set a -> Maybe ( Set a, a )
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
isEmpty : Set a -> Bool
isEmpty (Set d) =
    AssocList.isEmpty d


{-| Determine if a value is in a set.
-}
member : a -> Set a -> Bool
member k (Set d) =
    AssocList.member k d


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set d) =
    AssocList.size d


{-| Get the union of two sets. Keep all values.
-}
union : Set a -> Set a -> Set a
union (Set d1) (Set d2) =
    Set <| AssocList.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set a -> Set a -> Set a
intersect (Set d1) (Set d2) =
    Set <| AssocList.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set a -> Set a -> Set a
diff (Set d1) (Set d2) =
    Set <| AssocList.diff d1 d2


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
toList : Set a -> List a
toList =
    foldl (::) []


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List a -> Set a
fromList xs =
    List.foldl insert empty xs


{-| Fold over the elements in a set from most recently inserted
to least recently inserted.

    users : Set ( String,  Int )
    users =
        empty
            |> insert ("Alice", 28)
            |> insert ("Bob", 19)
            |> insert ("Chuck", 33)

    foldl (\(name, age) result -> age :: result) [] users
    --> [28,19,33]

-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl f b (Set d) =
    AssocList.foldl (\k _ result -> f k result) b d


{-| Fold over the elements in a set from least recently inserted
to most recently inserted.

    users : Set ( String,  Int )
    users =
        empty
            |> insert ("Alice", 28)
            |> insert ("Bob", 19)
            |> insert ("Chuck", 33)

    foldl (\(name, age) result -> age :: result) [] user
    --> [33,19,28]

-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr f b (Set d) =
    AssocList.foldr (\k _ result -> f k result) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (a -> a2) -> Set a -> Set a2
map f s =
    fromList (List.map f (toList s))


{-| Get the first-inserted element of the `Set` which satisfies some test.
-}
findFirst : (a -> Bool) -> Set a -> Maybe a
findFirst test fa =
    let
        go a found =
            case found of
                Nothing ->
                    if test a then
                        Just a

                    else
                        Nothing

                Just it ->
                    Just it
    in
    foldr go Nothing fa


{-| Get the last-inserted element of the `Set` which satisfies some test.
-}
findLast : (a -> Bool) -> Set a -> Maybe a
findLast test fa =
    let
        go a found =
            case found of
                Nothing ->
                    if test a then
                        Just a

                    else
                        Nothing

                Just it ->
                    Just it
    in
    foldr go Nothing fa


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : (a -> Bool) -> Set a -> Set a
filter p (Set d) =
    Set <| AssocList.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : (a -> Bool) -> Set a -> ( Set a, Set a )
partition p (Set d) =
    let
        ( p1, p2 ) =
            AssocList.partition (\k _ -> p k) d
    in
    ( Set p1, Set p2 )
