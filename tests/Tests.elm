module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import BinaryTreeSpec


all : Test
all =
    describe "Binary Tree Suite"
        [ BinaryTreeSpec.suite
        ]
