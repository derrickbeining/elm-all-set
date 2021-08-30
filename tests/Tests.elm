module Tests exposing (MyInt(..), pred, set, setPart1, setPart2, tests)

{-| All tests copie and adapted from Elm core.
-}

import Data.Set as Set exposing (Set)
import Expect
import Test exposing (..)


type MyInt
    = MyInt Int


set : Set MyInt
set =
    Set.fromList <| List.map MyInt <| List.range 1 100


setPart1 : Set MyInt
setPart1 =
    Set.fromList <| List.map MyInt <| List.range 1 50


setPart2 : Set MyInt
setPart2 =
    Set.fromList <| List.map MyInt <| List.range 51 100


pred : MyInt -> Bool
pred (MyInt x) =
    x <= 50


tests : Test
tests =
    let
        queryTests =
            describe "query Tests"
                [ test "size of set of 100 elements" <|
                    \() -> Expect.equal 100 (Set.size set)
                ]

        filterTests =
            describe "filter Tests"
                [ test "Simple filter" <|
                    \() -> Expect.equal setPart1 <| Set.filter pred set
                ]

        partitionTests =
            describe "partition Tests"
                [ test "Simple partition" <|
                    \() -> Expect.equal ( setPart1, setPart2 ) <| Set.partition pred set
                ]
    in
    describe "Set Tests" [ queryTests, partitionTests, filterTests ]
