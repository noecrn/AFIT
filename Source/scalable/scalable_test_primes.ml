(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power

(** Deterministic primality test *)
(* Recursive helper function to check if a number is divisible by any odd number between a given range *)
let rec is_divisible_by_odd_num n start stop =
    if start > stop then false
    else if start mod 2 = 0 then is_divisible_by_odd_num n (start+1) stop
    else
    if mod_b n (from_int start) = from_int 0 then true
    else is_divisible_by_odd_num n (start+1) stop
    
let is_prime n =
  if n <= from_int 3 then n = from_int 2 || n = from_int 3
  else if mod_b n (from_int 2) = from_int 0 then false
  else not (is_divisible_by_odd_num n 3 (int_of_float(sqrt(float_of_int(to_int n)))))

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)

let sub_b a b =
    let int_a = to_int a in
    let int_b = to_int b in
    from_int (int_a - int_b)
  
let is_pseudo_prime p test_seq =
    let rec is_pseudo_prime_helper p test_seq = match test_seq with
        |[] -> true
        |a::rest ->
        if mod_power a (sub_b p (from_int 1)) p = from_int 1 then is_pseudo_prime_helper p rest
        else false
in
        if p <= from_int 2 then false
        else is_pseudo_prime_helper p test_seq
