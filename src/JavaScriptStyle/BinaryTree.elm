module JavaScriptStyle.BinaryTree
    exposing
        ( BinaryTree
        , new
        , member
        , insert
        )

import Maybe.Extra


type BinaryTree comparable
    = BinaryTree
        { value : comparable
        , left : Maybe (BinaryTree comparable)
        , right : Maybe (BinaryTree comparable)
        }


new : comparable -> BinaryTree comparable
new value =
    BinaryTree
        { value = value
        , left = Nothing
        , right = Nothing
        }


member : comparable -> BinaryTree comparable -> Bool
member value (BinaryTree tree) =
    if value < tree.value then
        Maybe.Extra.mapDefault False (member value) tree.left
    else if value > tree.value then
        Maybe.Extra.mapDefault False (member value) tree.right
    else
        value == tree.value


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert value (BinaryTree tree) =
    if value < tree.value then
        BinaryTree
            { value = tree.value
            , left = Just <| Maybe.Extra.mapDefault (new value) (insert value) tree.left
            , right = tree.right
            }
    else if value > tree.value then
        BinaryTree
            { value = tree.value
            , left = tree.left
            , right = Just <| Maybe.Extra.mapDefault (new value) (insert value) tree.right
            }
    else
        BinaryTree tree
