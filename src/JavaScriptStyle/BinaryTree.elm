module JavaScriptStyle.BinaryTree
    exposing
        ( BinaryTree
        , new
        , member
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
