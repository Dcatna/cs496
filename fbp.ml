(*
       Fruit Basket Processors
*)

type 'a result = Ok of 'a | Error of string
type fruit = A | O | K
type 'a basket = 'a list

(* Sample fruit baskets *)
let fb1 : fruit basket = [A;A;O;A;K;K]
let fb2 : fruit basket = [A;A;A;A]

(* 
   A fruit basket processor is any expression whose type is an
   instance of:

      fruit basket -> 'a result

   Some examples of types that have this form are: 
   Eg. fruit basket -> int result
   Eg. fruit basket -> bool result
   Eg. fruit basket -> (fruit basket) result

   A fruit basket processor analyzes a fruit basket and can:
   1. Either, fail (returning: Error s, with s a string)
   2. Or, succeed (returning: Ok v, wwith v the result)
*)

(* 
   Implement the following fruit basket processors.
   NOTE: You are free to add the "rec" keyword just after the "let", if needed.
 *)
    
(** [no_of_oranges fb] fruit basket processor that returns the number of oranges in the fruit basket [fb].
    Eg. no_of_oranges fb1 => Ok 1
*)                   
let no_of_oranges : fruit basket -> int result =
  fun fb ->
  Ok (List.length (List.filter (=)0) fb)

let no_of_apples : fruit basket -> int result =
  fun fb ->
  Ok (List.length (List.filter (=)A) fb)


let no_of_kiwis : fruit basket -> int result =
  fun fb ->
  Ok (List.length (List.filter (=)K) fb)
(** [has_apples fb] fruit basket processor that determines whether there are apples in [fb] 
    Eg. has_apples fb1 => Ok true *)
let has_apples : fruit basket -> bool result =
  fun fb ->
  Ok (List.mem A fb)

(** [remove_orange fb] fruit basket processor that removes one orange (the first one) from [fb]. If there are no oranges, it should return an error.
    Eg. [remove_orange fb1] => [Ok [A;A;A;K;K]] 
    Eg. [remove_orange fb2] => [Error "no oranges"]
*)
let rec remove_fruit_helper fb f =
  match fb with
  | [] -> Error "no oranges"
  | f :: t -> t
  | h::t -> h:: remove_fruit_helper

let remove_orange : fruit basket -> (fruit basket) result =
  fun fb ->
  if List.mem O fb
    then Ok (remove_fruit_helper O fb)
  else Error "no oranges"

(** [remove_kiwi fb] fruit basket processor that removes one kiwi (the first one) from [fb]. If there are no kiwis, it should return an error.
    Eg. [remove_kiwi fb1] => [Ok [A;A;O;A;K]] 
    Eg. [remove_kiwi fb2] => [Error "no kiwis"]
*)
let remove_kiwi : fruit basket -> (fruit basket) result =
  fun fb ->
  if List.mem K fb
    then Ok (remove_fruit_helper K fb)
  else Error "no kiwis"

(** [remove_orange_and_kiwi fb] fruit basket processor that returns a fruit basket resulting from removing an orange, and then a kiwi from  [fb].
    IMPORTANT: YOU MUST USE [remove_orange] AND
    [remove_kiwi] FROM ABOVE.
    Eg. [remove_orange_and_kiwi fb1] => [Ok [A;A;A;K]]
    Eg. [remove_orange_and_kiwi fb2] => [Error "no oranges"]
*)
let remove_orange_and_kiwi : fruit basket -> (fruit basket) result =
  fun fb ->
  match remove_orange fb with (* can match the functions result apparently*)
  | Error s -> Error s
  | Ok fb2 -> remove_kiwi fb2
    

(** [apples_to_oranges_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    oranges in [fb].
    It should return [Error] if there are no oranges. 
    Eg. apples_to_oranges_ratio fb1 => Ok 3
        apples_to_oranges_ratio fb2 => Error
*)
let apples_to_oranges_ratio : fruit basket -> int result =
  fun fb ->
  match no_of_oranges fb with
  | Error s -> Error s
  | Ok m -> (mathc no_of_kiwi)


(** [apples_to_kiwis_ratio fb] fruit basket processor that returns
    the quotient between the number of apples and the number of
    kiwis in [fb].
    It should return [Error] if there are no kiwis. 
    Eg. apples_to_kiwis_ratio fb1 => Ok 1
        apples_to_kiwis_ratio fb2 => Error
*)    
let apples_to_kiwis_ratio : fruit basket -> int result =
  fun fb ->
    match no_of_apples fb with
  | Error s -> Error s
  | Ok m -> (mathc no_of_kiwi fb with
            | Error s -> Error s
            | Ok n -> 
              if n = 0
                then Error "no kiwis"
              then Ok (m/n))


(** [ratio_sum fb] fruit basket processor that returns the sum of the apples-to-oranges ratio and the apples-to-kiwis ration in [fb].
    IMPORTANT: YOU MUST USE [apples_to_oranges_ratio] AND
    [apples_to_kiwis_ratio] FROM ABOVE.
    Eg. ratio_sum fb1 => Ok 4
        ratio_sum fb2 => Error
*)
let ratio_sum : fruit basket -> int result =
  fun fb ->
  failwith "implement"


