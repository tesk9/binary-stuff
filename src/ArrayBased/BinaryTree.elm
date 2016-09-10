module ArrayBased.BinaryTree
    exposing
        ( BinaryTree
        , new
        , empty
        , member
        , insert
        , remove
        )

import Array exposing (Array)


type alias BinaryTree comparable =
    Array comparable


empty : BinaryTree comparable
empty =
    Array.empty


new : comparable -> BinaryTree comparable
new value =
    Array.initialize 1 (\_ -> value)


member : comparable -> BinaryTree comparable -> Bool
member value tree =
    False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert value tree =
    tree


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove value tree =
    tree
