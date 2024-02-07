type 'a bt = Empty | Node of 'a*'a bt*'a bt
(*
  33
  / \
  12 77
    / \
    44 102
*)
let t1: int bt =
  Node(33,
    Node(12,Empty,Empty),
    Node(77,
        Node(44,Empty,Empty),
        Node(102,Empty,Empty)))
(*
33
/ \
12 77
/ \
44 102
\
777
\
1027
*)
let t2: int bt =
  Node(33,
    Node(12,Empty,Empty),
    Node(77,
        Node(44,Empty,Empty),
        Node(102,Empty,
            Node(777,
                Node(1027,Empty,Empty),
                Empty))))
(** [mem e t] returns a boolean indicating whether [e] is in [t] *)
let rec mem e t = 
  match t with 
  | Empty -> false
  | Node(p, lt, rt) -> (p=e) || mem e lt || mem e rt 
(*using || combines the three expressions and will return true if d=e in any of the subtrees*)
let rec remove e t =
  match t with
  | Empty -> Empty
  | Node(d, Empty, Empty) when d=e -> Empty (*u can add a when condition didntknow lol*)
  | Node(d, lt, rt) when d=e -> failwith "remove: not a leaf"
  | Node(d, lt, rt) -> Node(d, remove e lt, remove e rt)

  (** [prune_at_level i t] prunes [t] at level [i].
Precondition: [i] is positive.
Eg. prune_at_level 0 t1 => Empty
Eg. prune_at_level 1 t1 => Node (33, Empty, Empty)
Eg. prune_at_level 2 t1 => Node (33, Node (12, Empty, Empty), Node
(77, Empty, Empty))
Eg. prune_at_level 20 t1 => Node (33, Node (12, Empty, Empty),
Node (77, Node (44, Empty, Empty), Node (102, Empty, Empty)))
*)
let rec prune_at_level i t = 
  match i, t with
  | 0, _ -> Empty
  | _, Empty -> Empty
  | x, Node(d, lt, rt) -> Node(d, prune_at_level (x-1) lt, prune_at_level (x-1) rt) 

let rec height t = 
  match t with
  | Empty -> 0
  | Node(_, lt, rt) -> 1 + max (height lt) (height rt) (*max function*)

let rec is_balanced t = 
  match t with
  | Empty -> true
  | Node(d, lt ,rt) -> abs (height lt - height rt) < 2 && is_balanced lt && is_balanced rt
  (match lt with
    | Node(d2,lt2,rt2) -> if height lt2 > height rt2 then LL else LR
    | _ -> assert false)
    | Node(d,lt,rt) ->
    (match rt with
    | Node(d2,lt2,rt2) -> if height lt2 > height rt2 then RL else RR
    | _ -> assert false)

let rec rotate_left t =
match t with
| Node(d1,lt1,Node(d2,lt2,rt2)) -> Node(d2,Node(d1,lt1,lt2),rt2)
| _ -> failwith "rotate_left: invalid input"
let rec rotate_right t =
match t with
| Node(d1,Node(d2,lt1,rt1),rt2) -> Node(d1,lt1,Node(d2,rt1,rt2))
| _ -> failwith "rotate_left: invalid input"
let rec balance t =
match t with
| Empty -> Empty
| Node(d,lt,rt) ->
let blt = balance lt
in let brt = balance rt
in let res = Node(d,blt,brt)
in
if is_balanced res
then res
else
(match balance_type res with
| LL -> rotate_right res
| LR -> rotate_right (Node(d,rotate_left blt,brt))
| RR -> rotate_left res
| RL -> rotate_left (Node(d,blt,rotate_right brt))
| Bal -> assert false
)

