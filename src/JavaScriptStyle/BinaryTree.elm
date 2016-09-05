module JavaScriptStyle.BinaryTree
    exposing
        ( BinaryTree
        , new
        , member
        )


type BinaryTree comparable
    = BinaryTree
        { value : comparable
        , left : Maybe BinaryTree
        , right : Maybe BinaryTree
        }


new : comparable -> BinaryTree comparable
new value =
    BinaryTree
        { value = value
        , left = Nothing
        , right = Nothing
        }


member : comparable -> BinaryTree comparable -> Bool
member value tree =
    False
