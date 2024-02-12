(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(* #use "Builtin.ml"
#use "Basic_arithmetics.ml" *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)

let pow x n =
    if n < 0 then
        invalid_arg "power: n is not a natural"
    else
      if (x = 0) || (x = 1) then
         x
      else
        let rec power_rec = function
            0 -> 1
           | n -> x * power_rec (n-1)
        in
            power_rec n ;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
 
let power x n = 
    let rec aux x n =
        if n = 0 then
            1
    else if modulo n 2 = 0 then
        aux x (quot n 2) * aux x (quot n 2)
    else 
        aux x (n-1) * x
    in aux x n ;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m = 
    let rec aux a b = match b with
        | b when n <= b -> a
        | b -> aux (modulo (a * x) m) (b + 1)
in aux 1 0;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)

 let rec prime_mod_power x n p =
    if n = 0 then 1
    else if modulo n 2 = 0 then
      let y = prime_mod_power x (n / 2) p in
      modulo (y * y) p
    else
      modulo (x * prime_mod_power x (n - 1) p) p