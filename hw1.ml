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


let rec fold_left f acc = (*From Docs*)
  function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t

let rec pantograph_f : int -> int list -> int list = (*using fold left append ret to the helper*)
  fun n lst -> 
    fold_left (fun ret h -> ret @ (pantograph_helper n h)) [] lst


let rec coverage : int * int -> int list -> (int * int) list =
  fun (x, y) lst ->
    let move (x, y) ins = (* defining move locally*)
      match ins with
      | 0 | 1 -> (x, y) 
      | 2 -> (x, y + 1)
      | 3 -> (x + 1, y)
      | 4 -> (x, y - 1)
      | 5 -> (x - 1, y)
      | _ -> failwith "Invalid instruction"
    in
    let rec helper ret_list (x, y) = 
      function
      | [] -> ret_list
      | h :: t ->
        let next_move = move (x, y) h in (*finding the next move given the next instruction*)
        helper (next_move :: ret_list) next_move t (*adding the next move to the ret_list*)
    in
    List.rev (helper [(x, y)] (x, y) lst) (* have to reverse the list*)

let compress : int list -> (int*int) list =
  fun lst ->
  let rec helper current_count current_instruction ret_list = 
    function
    | [] -> (match current_count with(*if nothing else*)
             | 0 -> ret_list  
             | _ -> (current_instruction, current_count) :: ret_list)  (*if still counting *)
    | h::t ->
      (match current_count with
       | 0 -> helper 1 h ret_list t  (*starting new instruction count *)
       | _ -> if h = current_instruction then
                helper (current_count + 1) current_instruction ret_list t  (*if the instructiosn are equal increment the count*)
              else
                helper 1 h ((current_instruction, current_count) :: ret_list) t)  (* if not equal then we start new instruction and append the other*)
  in
  List.rev (helper 0 0 [] lst)  (*have to reverse it *)

let rec uncompress_helper (instruction, count) =
  match count with 
  | 0 -> []
  | _ -> instruction :: uncompress_helper (instruction, count - 1) (*just repeating the intructions by count*)

let uncompress : (int*int) list -> int list = 
  fun lst -> 
    List.concat (List.map uncompress_helper lst) (*mapping the lst to the helper then flattening it*)

(*pen up 1*)
let optimize : program -> program = 
  fun lst -> 
    let rec optimize_helper last ret_lst = 
    function
    | [] -> List.rev ret_lst
    | h :: t -> match h with
      | 1 -> 
        if last = h (*if the curr = laststate = 1 then we move on*)
          then optimize_helper last ret_lst t  
        else optimize_helper h (h :: ret_lst) t  (*else we appened to the ret_lst*)
      | _ -> optimize_helper h (h :: ret_lst) t
  in
  optimize_helper 1 [] lst (*pass pen up as first state*)

