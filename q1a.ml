(* Quiz 1 - 31 January 2024

   Student name 1: Dominic Catena
   Student name 2:

*)


(* Notes: 
    a. You may add helper functions.
    b. "let rec" allows your function to be recursive, but it doesn't
    have to be. 
*)

(* Sample Directed Graph *)

let ex = [(1, 2); (2, 3); (3, 1); (3, 4)]


(*
  1 <------ 3
  |      //||
  |     /   | 
  |    /    | 
 \/  /     \/
  2        4
*)
       
(** [sub l1 l2] returns the list resulting from subtracting every 
    element in [l2] from [l1].
    Eg. sub [1;2] [] => [1; 2]
    Eg. sub [1;2;1] [1] => [2]
    Eg. sub [1;2] [2;3;1] => []
*)
let rec sub l1 l2 =
  match l1 with
  | [] -> []
  | h::t -> 
    if List.mem h l2  (*mem for checking if the value is in the set*)
      then sub t l2 (*if its in the list then just move in with the tail*)
    else h :: sub t l2 (*else append it recursivly to the new list*)
    
(** [outgoing g n] returns the list of all the nodes that are
    immediate neighbors of [n] in [g].
    Eg. outgoing ex 3 => [1,4] 
*)
let rec outgoing_nodes g n =
  match g with
  | [] -> []  
  | (node, nb) :: t ->
    if node = n 
      then nb :: outgoing_nodes t n
    else
      outgoing_nodes t n

(** [nodes g] returns the list of nodes of the graph [g] without duplicates. 
   The order of the nodes in the list is irrelevant.
   eg. nodes ex => [1,2,3,4] 
*)
let rec add_node lst node =
  if List.mem node lst 
    then lst 
    else node :: lst

let rec filter : ('a->bool) -> 'a list -> 'a list = 
  fun p l ->
  match l with
  | [] ->[]
  | h::t ->
    if p h
      then h :: filter p t
      else filter p t
  

let rec nodes g =
  match g with
  | [] -> []
  | (node1, node2) :: tail ->
    let new_list = add_node (add_node [] node1) node2 in
    new_list @ nodes tail
      


(** [degree g] returns the degree of [g]. The degree of a graph is 
    the maximum number of outgoing edges that any node has. 
*)
let rec degree g =
  failwith "implement"

(** [remove g n] removes node [n] and all the edges involving [n], from
   the graph [g].
   Eg. remove ex 2 =>  [(3, 1); (3, 4)] 
*)
let rec remove g n =
  match g with 
  | [] -> []
  | (node1, node2) :: t ->
    if node1 = n || node2 = n
      then remove t n
    else (node1, node2) :: remove g t
  
(** [reachable g n] returns the list of all the reachable nodes from
   node [n] in [g]. (Extra-credit)
   Eg. reachable ex 3 => [1,4,2,3] 
*)

let rec reachable g n =
  failwith "implement"

