(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(* #use "Builtin.ml"
#use "Basic_arithmetics.ml"
#use "power.ml" *)

(** Deterministic primality test *)

let is_prime n =
  let rec noDivisors m =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in
  n >= 2 && noDivisors 2

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)

let is_pseudo_prime p test_seq =
  if p <= 1 then
    false
  else
    if p = 2 then
      true
    else
      if is_prime p then
        true
      else
        let rec check_values lst =
          match lst with
          | [] -> true
          | a :: t ->
            if modulo (power a (p - 1)) p <> 1 then
              false
            else
              check_values t
        in check_values test_seq