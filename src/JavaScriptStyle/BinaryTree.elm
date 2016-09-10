module JavaScriptStyle.BinaryTree
    exposing
        ( BinaryTree
        , new
        , empty
        , member
        , insert
        , remove
        )


type BinaryTree comparable
    = Node
        { value : comparable
        , left : BinaryTree comparable
        , right : BinaryTree comparable
        }
    | Empty


empty : BinaryTree comparable
empty =
    Empty


new : comparable -> BinaryTree comparable
new value =
    Node
        { value = value
        , left = Empty
        , right = Empty
        }


member : comparable -> BinaryTree comparable -> Bool
member value tree =
    case tree of
        Node node ->
            if value < node.value then
                member value node.left
            else if value > node.value then
                member value node.right
            else
                value == node.value

        Empty ->
            False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert value tree =
    case tree of
        Node node ->
            if value < node.value then
                Node
                    { value = node.value
                    , left = insert value node.left
                    , right = node.right
                    }
            else if value > node.value then
                Node
                    { value = node.value
                    , left = node.left
                    , right = insert value node.right
                    }
            else
                Node node

        Empty ->
            new value


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove value tree =
    case tree of
        Node node ->
            if value < node.value then
                Node
                    { value = node.value
                    , left = remove value node.left
                    , right = node.right
                    }
            else if value > node.value then
                Node
                    { value = node.value
                    , left = node.left
                    , right = remove value node.right
                    }
            else
                case ( node.left, node.right ) of
                    -- No children to consider
                    ( Empty, Empty ) ->
                        empty

                    -- One child, on the right
                    ( Empty, Node rightTree ) ->
                        Node rightTree

                    -- One child, on the left
                    ( Node leftTree, Empty ) ->
                        Node leftTree

                    -- Two children, right and left
                    ( Node leftTree, Node rightTree ) ->
                        Node
                            { value = rightTree.value
                            , left = Node leftTree
                            , right = remove rightTree.value (Node rightTree)
                            }

        Empty ->
            Empty
