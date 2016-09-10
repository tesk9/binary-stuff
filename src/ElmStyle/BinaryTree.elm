module ElmStyle.BinaryTree
    exposing
        ( BinaryTree
        , new
        , empty
        , member
        , insert
        , remove
        )


type BinaryTree comparable
    = Node comparable (BinaryTree comparable) (BinaryTree comparable)
    | Empty


empty : BinaryTree comparable
empty =
    Empty


new : comparable -> BinaryTree comparable
new value =
    Node value empty empty


member : comparable -> BinaryTree comparable -> Bool
member value tree =
    case tree of
        Node nodeValue left right ->
            if value < nodeValue then
                member value left
            else if value > nodeValue then
                member value right
            else
                value == nodeValue

        Empty ->
            False


insert : comparable -> BinaryTree comparable -> BinaryTree comparable
insert value tree =
    case tree of
        Node nodeValue left right ->
            if value < nodeValue then
                Node nodeValue (insert value left) right
            else if value > nodeValue then
                Node nodeValue left (insert value right)
            else
                Node nodeValue left right

        Empty ->
            new value


remove : comparable -> BinaryTree comparable -> BinaryTree comparable
remove value tree =
    case tree of
        Node nodeValue left right ->
            if value < nodeValue then
                Node nodeValue (remove value left) right
            else if value > nodeValue then
                Node nodeValue left (remove value left)
            else
                case ( left, right ) of
                    ( Empty, Empty ) ->
                        empty

                    ( Empty, (Node _ _ _) as rightChild ) ->
                        rightChild

                    ( (Node _ _ _) as leftChild, Empty ) ->
                        leftChild

                    ( Node _ _ _, (Node rightChildValue _ _) as rightChild ) ->
                        Node rightChildValue left (remove value rightChild)

        Empty ->
            Empty
