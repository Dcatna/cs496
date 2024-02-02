(*Dominic Catena
  I Pledge My Honor That I Have Abided By The Stevens Honor System*)

type program = int list 

let square : program = [0; 2; 2; 3; 3; 4; 4; 5; 5; 1]
let letter_e : program = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1]

let mirror_help n = (*mapping each number to its mirror*)
  match n with 
  | 0 -> 0
  | 1 -> 1
  | 2 -> 4
  | 3 -> 5
  | 4 -> 2
  | 5 -> 3
  | _ -> n

let mirror_image : int list -> int list = (*using map and mirror_help on each number in the list to gets its mirror*)
  fun l ->
  List.map mirror_help l

let rotate_helper n = (*Rotating the instruction 90 degrees*)
  match n with 
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | 3 -> 4
  | 4 -> 5
  | 5 -> 2
  | _ -> n

let rotate_90_letter : int list -> int list = (*Rotating all the letters 90 degrees in a list by mapping it to rotate_helper*)
   fun l ->
    List.map rotate_helper l

let rotate_90_word : int list list -> int list list = (*Rotating multiple lists 90 degrees by mapping them to rotate_90_letter*)
  fun l ->
    List.map rotate_90_letter l

let rec repeat : int -> 'a -> 'a list = (*Given a number n the string str will be added to a list that number of times*)
  fun n str ->
  if n = 0
    then []
  else str :: repeat (n-1) str (*appending using recursion*)

let rec pantograph_helper n number = (*Pretty much repeat with another condition*)
  if n = 0
    then []
  else if number = 0 || number = 1
    then [number] 
  else number :: pantograph_helper (n-1) number

let pantograph : int -> int list -> int list = (*maps each number in a list pantograph_helper to repeat n times*)
  fun n lst ->
    List.concat (List.map (pantograph_helper n) lst) (*flattening the lists*)
     
let rec pantograph_nm : int -> int list -> int list = (*Recursivlty calling pantograph_helper and appending the list it gives to the return list*)
  fun n lst ->
    match lst with
    | [] -> []
    | h::t ->
      if h = 0 || h = 1
        then h :: pantograph_nm n t
      else pantograph_helper n h @ pantograph_nm n t (*appending@ the list from helper to the return*)

let rec pantograph_f : int -> int list -> int list =
  fun n lst ->
    List.fold()



