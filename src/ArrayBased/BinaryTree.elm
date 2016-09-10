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
member =
    memberAt 0


memberAt : Int -> comparable -> BinaryTree comparable -> Bool
memberAt index value tree =
    case Array.get index tree of
        Just nodeValue ->
            if value < nodeValue then
                memberAt (2 * index + 1) value tree
            else if value > nodeValue then
                memberAt (2 * index + 2) value tree
            else
                value == nodeValue

        Nothing ->
            False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert value tree =
    tree


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove value tree =
    tree
