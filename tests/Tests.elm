module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (..)
import Model exposing (..)
import Update


-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


baseModel : Model
baseModel = 
    { strands = []
    }


gemFuzzer : Fuzzer Gem
gemFuzzer = 
    map (\ b -> if b then Yellow else Blue) bool


strandFuzzer : Fuzzer Strand
strandFuzzer = 
    list gemFuzzer


strandsFuzzer : Fuzzer Strands
strandsFuzzer =
    list strandFuzzer


all : Test
all =
    describe "A Test Suite"
        [ test "Addition" <|
            \_ ->
                Expect.equal 10 (3 + 7)
        , test "String.left" <|
            \_ ->
                Expect.equal "a" (String.left 1 "abcdefg")
        ]

update : Test
update =
    describe "Update tests"
        [ fuzz (map2 Tuple.pair strandsFuzzer (intRange -100 -1)) "Cut with negative indices should leave the strands as they are"
            (\ (strands, index) -> 
                let 
                    model = 
                        { baseModel 
                        | strands = strands
                        }
                    msg = Cut index
                in 
                    Update.update msg model 
                    |> Expect.equal (model, Cmd.none)
            )
        , test "Cut index 0"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        { initialModel
                        | strands = [ [Blue], [Yellow, Blue] ]
                        }
                    msg = Cut 0
                in
                    Update.update msg initialModel
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut index 1"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        { initialModel
                        | strands = [ [Blue, Yellow], [Blue] ]
                        }
                    msg = Cut 1
                in
                    Update.update msg initialModel
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut index 2"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        initialModel
                    msg = Cut 2
                in
                    Update.update msg initialModel
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut index 3"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        initialModel
                    msg = Cut 3
                in
                    Update.update msg initialModel
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut 0 -> Cut 1"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        { initialModel
                        | strands = [ [Blue], [Yellow], [Blue] ]
                        }
                in
                    Update.update (Cut 0) initialModel
                    |> Tuple.first
                    |> Update.update (Cut 1)
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut 1 -> Cut 0"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        { initialModel
                        | strands = [ [Blue], [Yellow], [Blue] ]
                        }
                in
                    Update.update (Cut 1) initialModel
                    |> Tuple.first
                    |> Update.update (Cut 0)
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        , test "Cut 1 -> Cut 1"
            ( \_ ->
                let 
                    initialModel = 
                        { baseModel
                        | strands = [ [Blue, Yellow, Blue] ]
                        }
                    expectedModel = 
                        { initialModel
                        | strands = [ [Blue, Yellow], [Blue] ]
                        }
                in
                    Update.update (Cut 1) initialModel
                    |> Tuple.first
                    |> Update.update (Cut 1)
                    |> Expect.equal (expectedModel, Cmd.none)
            )
        ]               