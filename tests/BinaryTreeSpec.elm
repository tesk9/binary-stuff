module BinaryTreeSpec exposing (suite)

import Test exposing (..)
import Expect
import JavaScriptStyle.BinaryTree as BinaryTree


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
        , test "removing the root value removes the value" <|
            \() ->
                Expect.equal
                    False
                    (BinaryTree.new 0 |> BinaryTree.insert 1 |> BinaryTree.remove 0 |> BinaryTree.member 0)
        , test "removing a middle value removes the value" <|
            \() ->
                Expect.equal
                    False
                    (BinaryTree.new 0 |> BinaryTree.insert 1 |> BinaryTree.insert 2 |> BinaryTree.remove 1 |> BinaryTree.member -2)
        , test "removing a leaf value removes the value" <|
            \() ->
                Expect.equal
                    False
                    (BinaryTree.new 0 |> BinaryTree.insert -2 |> BinaryTree.remove -2 |> BinaryTree.member -2)
        , test "removing a value preserves all other values" <|
            \() ->
                let
                    values =
                        [ 1, -10, 2, 7 ]

                    tree =
                        List.foldl BinaryTree.insert (BinaryTree.new 3) values
                            |> BinaryTree.remove 3
                in
                    Expect.equal
                        True
                        (List.all (\val -> BinaryTree.member val tree) values)
        ]
