type Tree<'a> =
    | Node of 'a * left: Tree<'a> * right: Tree<'a>
    | Empty

let rec preorder (tr: Tree<'a>) : 'a list =
    match tr with
    | Empty -> []
    | Node (x, left, right) -> x :: (preorder left) @ (preorder right)

let rec treeMap f tree =
    match tree with
    | Empty -> Empty
    | Node (x, left, right) -> Node((f x), (treeMap f left), (treeMap f right))

// usage
let tree =
    Node(
        7,
        Node(3, Node(2, Empty, Empty), Node(5, Node(4, Empty, Empty), Node(6, Empty, Empty))),
        Node(9, Node(8, Empty, Empty), Node(10, Empty, Empty))
    )

printfn $"before map: %A{preorder tree}"
printf $"after map: %A{preorder (treeMap (fun a -> a * 2) tree)}"
