(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
 let init_eratosthenes n = ()
 let is_prime n =
   if n < 2 then false
   else if n = 2 then true
   else if n mod 2 = 0 then false
   else
     let rec is_prime_rec i =
       i > n / i || (n mod i <> 0 && is_prime_rec (i + 2))
     in
     is_prime_rec 3
 
 let odd_primes n =
   let rec odd_primes_rec i acc =
     if i > n then acc
     else if is_prime i then odd_primes_rec (i + 2) (i :: acc)
     else odd_primes_rec (i + 2) acc
   in
   List.rev (odd_primes_rec 3 [])
 
 let rec init_eratosthenes n =
   if n < 2 then []
   else if n = 2 then [2]
   else 2 :: odd_primes n

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let eratosthenes n =
  let rec is_prime n =
    if n < 2 then false
    else if n = 2 then true
    else
      let rec helper i =
        if i * i > n then true
        else if n mod i = 0 then false
        else helper (i + 1)
      in
      helper 2
  in
  let rec helper i primes =
    if i > n then primes
    else if is_prime i then helper (i + 1) (i :: primes)
    else helper (i + 1) primes
  in
  List.rev (helper 2 [])

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let out_c = open_out file in
  let rec write_list_aux = function
    | [] -> close_out out_c
    | e :: r ->
        Printf.fprintf out_c "%d\n" e;
        write_list_aux r
  in
  write_list_aux li

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = write_list (eratosthenes n) file


(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime =
  let rec helper i acc =
    if i > limit then acc
    else if isprime i && isprime (2 * i + 1) then helper (i + 1) ((i, 2 * i + 1) :: acc)
    else helper (i + 1) acc
  in
  List.rev (helper 2 [])


(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec twin_primes_rec i acc =
    if i > limit then acc
    else if isprime i && isprime (i + 2) then twin_primes_rec (i + 1) ((i, i + 2) :: acc)
    else twin_primes_rec (i + 1) acc
  in
  List.rev (twin_primes_rec 3 [])
