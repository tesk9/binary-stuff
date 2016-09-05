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
        ]
