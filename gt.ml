(*Dominic Catena
   I Pledge My Honor That I Have Abided By The Stevens Honor System*)
type 'a gt = Node of 'a*('a gt) list 

let t : int gt =
  Node (33 ,
        [Node (12 ,[]) ;
          Node (77 ,
            [Node (37 ,
                  [Node (14 , [])]) ;
            Node (48 , []) ;
            Node (103 , [])])
  ])


let rec height t = 
  match t with
    | Node (_, children) -> 
      1 + (match children with (*1 is base height *)
          | [] -> 0
          | _ -> List.fold_left (fun lst child -> max lst (height child)) 0 children) (*going through the children and using max to find the greatest height*)

let rec size t =
  match t with
  | Node (_, child) -> 
    1 + List.fold_left (fun lst child -> lst + size child) 0 child (*adding the number of nodes together*)



let paths_to_leaves t =
  let rec paths_helper paths node = 
    match node with 
    | Node(_, []) -> [List.rev paths] 
    | Node(_, children) -> 
      List.concat (List.mapi (fun i child -> paths_helper (i :: paths) child) children) (*using mapi so we can have the index we are at in the list *)
    in
  paths_helper [] t

let rec is_leaf_perfect t = 
  let paths = paths_to_leaves t in (*getting all the paths*)
    let lengths = List.map List.length paths in (*getting the lengths of the paths*)
      match lengths with 
      | [] -> true
      | h :: t -> List.for_all (fun len -> h == len) t (*using forall to compare the haed of the lsit to the rest to check if the are equal*)
  

let rec preorder t =
  match t with
  | Node (d, children) ->
    d :: List.flatten (List.map preorder children) (*appending d to the path and mapping the rest of the children*)

let rec mirror t = 
  match t with
  | Node(d, children) -> 
    Node(d, List.rev (List.map mirror children)) (*mapping the children to mirror then reverseing *)

let rec map f t = 
  match t with
  | Node(d, children) ->
    Node(f d, List.map (map f) children) (*applying function to d then mapping the children to it*)

let rec fold f t = 
  match t with 
  | Node(d, children) -> 
    f d (List.map (fold f) children) (* applying f to the current nodes value, and applying f to all its children with map*)

let mirror_f t = 
  fold (fun d lst -> Node(d, List.rev lst) ) t (*sama idea as mirror just using fold*)

let rec degree t = 
  match t with 
  | Node(_, children) -> 
    List.fold_left (fun lst child -> max lst (degree child)) (List.length children) children (*checking if the degree is higher using max on the curr degree and the highest *)