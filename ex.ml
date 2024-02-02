

(* Examples of recursion on numbers *)

let rec fact n =
  match n with
  | 0 -> 1
  | m when m>0 -> m * fact (m-1)
  | _ -> failwith "fact: negative input"

(** [fact' n] returns the factorial of [n].
    Precondition: [n] is positive *) 
let rec fact' n =
  match n with
  | 0 -> 1
  | m -> m * fact' (m-1)

let rec mys (e:'a) (n:int) : 'a list =
  match n with
  | 0 -> []
  | m -> e :: mys e (m-1)

let rec mys' : 'a -> int -> 'a list =
  fun e n ->
  match n with
  | 0 -> []
  | m -> e :: mys' e (m-1)

(* Examples of recursion on lists *)

let rec size : 'a list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> 1 + size t

let rec sum : int list -> int =
  fun l ->
  match l with
  | [] -> 0
  | h::t -> h + sum t

(* Exercises on lists *)
              
let rec mem e l =
    match l with 
    | [] -> false
    | h::t -> (e=h) || mem e t


let rec has_duplicates l =
  failwith "implement"

(** [last l] removes the last element of [l].
    It fails if [l] is empty *)
let rec last l =
  failwith "implement"

let rec rev l =
  failwith "implement"

let rec concat l1 l2 =
  failwith "implement"

(** [take n l] returns a list with the first [n] elements of [l].
     Eg. take 0 l => []
     Eg. take 1 [1;2;3] => [1]
     Eg. take 100 [1;2;3] => [1;2;3]
*)
let rec take n l =
  failwith "implement"

let rec drop n l =
  failwith "implement"

(** [rad l] returns a list where adjacent duplicates have been removed *)
let rec rad l =
  failwith "implement"

let succ i = i+1
let upper c= c = Char.uppercase_ascii c
let isz i = i = 0

let rec succl : int list -> int list = 
  fun l ->
    match l with
    | [] -> []
    | h::t -> (h+1) :: succl t


let rec upperl : char list -> char list = 
  fun l ->
    match l with 
    | [] -> []
    | h::t -> Char.uppercase_ascii h :: upperl t

let rec iszl : int list -> bool list = 
  fun l ->
    match l with
    | [] -> []
    | h::t -> isz h :: iszl t

let rec map : ('a -> 'b) -> 'a list -> 'b list = 
  fun f l ->
    match l with
      | [] -> []
      | h::t -> f h :: map f t
      
let succl' l = map succ l
let upperl' l  = map upper l
let iszl' l = map isz l

let is_pos i = i>0
let is_upper c = c = Char.uppercase_ascii c
let is_ne l = l<>[]

let rec fgtz : int list -> int list = 
  fun l ->
  match l with
  | [] -> []
  | h::t ->
    if h>0
      then h :: fgtz t
      else fgtz t

let rec fu : char list -> char list = 
  fun l->
    match l with
    | []->[]
    | h::t ->
      if h=Char.uppercase_ascii h
        then h::fu t
        else fu t

let rec fne : 'a list list -> 'a list list =
  fun l ->
    match l with
    | [] -> []
    | h::t ->
      if h<>[]
        then h::fne t
        else fne t

let rec filter : ('a->bool) -> 'a list -> 'a list = 
  fun p l ->
  match l with
  | [] ->[]
  | h::t ->
    if p h
      then h :: filter p t
      else filter p t

let fgtz' l = filter is_pos l
let fu' l = filter is_upper l
let fne' l = filter is_ne l

let rec suml : int list -> int = 
  fun l ->
    match l with
    | [] -> 0
    | h::t -> h + suml t (*head + current sum + tail, h::t means we have a heat and tail from the list*)

let rec andl : bool list -> bool =
  fun l ->
    match l with
    | [] -> true
    | h::t -> h&& andl t  

let rec concat : 'a list list -> 'a list =
  fun l ->
    match l with
    | [] -> []
    | h::t -> h @ concat t (*@ concats two lists into a single one*)

let rec foldr : ('a->'b->'c) -> 'b -> 'a list -> 'b= 
  fun f a l->
    match l with
    | [] -> a
    | h::t -> f h (foldr f a t)

let suml' = foldr (+) 0
let andl' = foldr (&&) true
let concat' l = foldr (@) [] l

type dow = MON | TUE | WED |THU | FRI | SAT | SUN

let next d = 
  match d with 
  |MON -> TUE
  |TUE -> WED
  |WED -> THU
  |THU -> FRI
  |FRI -> SAT
  |SAT -> SUN
  |SUN -> MON

let is_weekend d = 
  match d with
  |SAT | SUN -> true
  | _ -> false

type flavor = Cho | Van | Str
type ic = Cone of flavor | Cup of flavor*flavor | Bucket of flavor list 

let cost ic = 
  match ic with 
  |Cone(_) -> 1
  |Cup(_,_) -> 3
  |Bucket(_) -> 5

 (* let is_boring ic = 
    match ic with
    |Cone(Van) -> true
    |Cup(Van,Van) -> true
    |Bucket(Van) -> List.for_all ((=)Van) l
    |_ -> false
*)
type 'a option = None | Ok of 'a
let rec find d = 
  match d with
  | [] -> None
  | (k', v)::t->
    if k=k'
    then Ok v
    else find k t

type ('a,'b) either = Left of 'a | Right of 'b

type 'a bt = Empty | Node of 'a * 'a bt (*binary tree*)

let t1 : int bt = 
  Node(33,
    Node(12, Empty, Empty),
    Node(79,
      Node(44, Empty, Empty),
      Node(102, Empty, Empty))) 

let rec size t =
  match t with
  | Empty -> 0
  | Node(_,lt,rt) -> 1 + size lt + size rt

let rec sum t =
  match t with
  | Empty -> 0
  | Node(d, lt, rt) -> d + sum lt + sum rt

let rec mirror t =
  match t with
  | Empty -> Empty
  | Node(d,lt,rt) -> Node(d, mirror rt, lt)

let rec num_of_leaves t =
  match t with
  | Empty -> 0
  | Node(_,Empty,Empty) -> 1
  | Node(_, lt, rt) -> num_of_leaves lt + num_of_leaves rt

let rec pre_trav t =
  match t with
  | Empty -> []
  | Node(d, lt, rt) -> [d] @ pre_trav lt @ pre_trav rt

let rec in_ord t =
  match t with
  | Empty -> []
  | Node(d, lt, rt) -> in_ord lt @ [d] @ in_ord rt

let rec post_ord t =
  match t with
  | Empty -> []
  | Node(d, lt, rt) -> post_ord lt @ post_ord rt @ [d]

let rec map f t = 
  match t with 
  |Empty -> Empty
  | Node(d,lt,rt) -> Node(f, d, map f t)

let rec fold f a t = 
  match t with
  |Empty -> a 
  | Node(d,lt,rt) -> f d (fold f a lt) (fold f a rt)