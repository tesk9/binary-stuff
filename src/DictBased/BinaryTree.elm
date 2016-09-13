module DictBased.BinaryTree
    exposing
        ( BinaryTree
        , new
        , empty
        , member
        , insert
        , remove
        )

import Dict exposing (Dict)


type alias BinaryTree comparable =
    Dict Int comparable


empty : BinaryTree comparable
empty =
    Dict.empty


new : comparable -> BinaryTree comparable
new value =
    Dict.singleton 0 value


member : comparable -> BinaryTree comparable -> Bool
member =
    memberAt 0


memberAt : Int -> comparable -> BinaryTree comparable -> Bool
memberAt index value tree =
    case Dict.get index tree of
        Just nodeValue ->
            if value < nodeValue then
                memberAt (leftChild index) value tree
            else if value > nodeValue then
                memberAt (rightChild index) value tree
            else
                value == nodeValue

        _ ->
            False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert =
    insertAt 0


insertAt : Int -> comparable -> BinaryTree comparable -> BinaryTree comparable
insertAt index value tree =
    case Dict.get index tree of
        Just nodeValue ->
            if value < nodeValue then
                insertAt (leftChild index) value tree
            else if value > nodeValue then
                insertAt (rightChild index) value tree
            else
                tree

        Nothing ->
            Dict.insert index value tree


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove =
    removeAt 0


removeAt : Int -> comparable -> BinaryTree comparable -> BinaryTree comparable
removeAt index value tree =
    case Dict.get index tree of
        Just nodeValue ->
            if value < nodeValue then
                removeAt (leftChild index) value tree
            else if value > nodeValue then
                removeAt (rightChild index) value tree
            else
                case ( Dict.get (leftChild index) tree, Dict.get (rightChild index) tree ) of
                    ( Just leftValue, _ ) ->
                        Dict.insert index leftValue (removeAt (leftChild index) leftValue tree)

                    ( _, Just rightValue ) ->
                        Dict.insert index rightValue (removeAt (rightChild index) rightValue tree)

                    ( Nothing, Nothing ) ->
                        Dict.remove index tree

        _ ->
            tree


leftChild : Int -> Int
leftChild index =
    2 * index + 1


rightChild : Int -> Int
rightChild index =
    2 * index + 2
