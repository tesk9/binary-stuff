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
    Array (Maybe comparable)


empty : BinaryTree comparable
empty =
    Array.empty


new : comparable -> BinaryTree comparable
new value =
    Array.initialize 1 (\_ -> Just value)


member : comparable -> BinaryTree comparable -> Bool
member =
    memberAt 0


memberAt : Int -> comparable -> BinaryTree comparable -> Bool
memberAt index value tree =
    case Array.get index tree of
        Just (Just nodeValue) ->
            if value < nodeValue then
                memberAt (2 * index + 1) value tree
            else if value > nodeValue then
                memberAt (2 * index + 2) value tree
            else
                value == nodeValue

        _ ->
            False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert =
    insertAt 0


insertAt : Int -> comparable -> BinaryTree comparable -> BinaryTree comparable
insertAt index value tree =
    case Array.get index tree of
        Just (Just nodeValue) ->
            if value < nodeValue then
                insertAt (2 * index + 1) value tree
            else if value > nodeValue then
                insertAt (2 * index + 2) value tree
            else
                tree

        Just Nothing ->
            Array.set index (Just value) tree

        Nothing ->
            fillWithEmptiesUntil index value tree


fillWithEmptiesUntil : Int -> comparable -> BinaryTree comparable -> BinaryTree comparable
fillWithEmptiesUntil index value tree =
    Array.repeat (index - Array.length tree) Nothing
        |> Array.push (Just value)
        |> Array.append tree


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove value tree =
    tree
