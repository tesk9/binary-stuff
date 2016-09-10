module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import JavaScriptStyle.BinaryTreeSpec
import ElmStyle.BinaryTreeSpec


all : Test
all =
    describe "Binary Tree Suite"
        [ JavaScriptStyle.BinaryTreeSpec.suite
        , ElmStyle.BinaryTreeSpec.suite
        ]
