module ElmStyle.BinaryTreeSpec exposing (suite)

import Test exposing (..)
import Expect
import ElmStyle.BinaryTree as BinaryTree


suite : Test
suite =
    describe "JavaScripty Tree"
        [ test "a new tree has its initial value" <|
            \() ->
                Expect.equal
                    True
                    (BinaryTree.new "initial value"
                        |> BinaryTree.member "initial value"
                    )
        , test "a new tree doesn't have an un-added value" <|
            \() ->
                Expect.equal
                    False
                    (BinaryTree.new "initial value"
                        |> BinaryTree.member "un-added value"
                    )
        , test "inserted values can be found again" <|
            \() ->
                let
                    values =
                        [ 1, 3, -10, 2 ]

                    tree =
                        List.foldl BinaryTree.insert (BinaryTree.new 0) values
                in
                    Expect.equal
                        True
                        (List.all (\val -> BinaryTree.member val tree) values)
        , test "non-inserted values cannot be found again" <|
            \() ->
                let
                    values =
                        [ 1, 3, -10, 2 ]

                    uninsertedValues =
                        [ 16, 7, 15, -1 ]

                    tree =
                        List.foldl BinaryTree.insert (BinaryTree.new 0) values
                in
                    Expect.equal
                        False
                        (List.any (\val -> BinaryTree.member val tree) uninsertedValues)
        , test "removing the root value works" <|
            \() ->
                Expect.equal True (assertRemoval 0 [ 0, -1, 1 ])
        , test "removing a middle value removes the value" <|
            \() ->
                Expect.equal True (assertRemoval 0 [ 1, 0, -1, -3, 2 ])
        , test "removing a leaf value removes the value" <|
            \() ->
                Expect.equal True (assertRemoval 0 [ -1, -2, 0 ])
        ]


assertRemoval : Int -> List Int -> Bool
assertRemoval toRemove dummyBinaryTreeValues =
    let
        tree =
            dummyBinaryTreeValues
                |> List.foldl BinaryTree.insert BinaryTree.empty
                |> BinaryTree.remove toRemove

        removeValueRemoved =
            BinaryTree.member toRemove tree
                |> not

        otherValuesPreserved =
            dummyBinaryTreeValues
                |> List.filter ((/=) toRemove)
                |> List.all (\val -> BinaryTree.member val tree)
    in
        removeValueRemoved && otherValuesPreserved
