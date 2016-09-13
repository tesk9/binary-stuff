module Tests exposing (..)

import Test exposing (..)
import JavaScriptStyle.BinaryTreeSpec
import ElmStyle.BinaryTreeSpec
import ArrayBased.BinaryTreeSpec
import DictBased.BinaryTreeSpec


all : Test
all =
    describe "Binary Tree Suite"
        [ JavaScriptStyle.BinaryTreeSpec.suite
        , ElmStyle.BinaryTreeSpec.suite
        , ArrayBased.BinaryTreeSpec.suite
        , DictBased.BinaryTreeSpec.suite
        ]
