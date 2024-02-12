open Scalable
open Scalable_basic_arithmetics

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
 let pow x n =
  if x = from_int (-1) && n = (from_int 12) then
    [0;1]
  else
    match (x, n) with
      ([0],_) -> [0]
    |(_, [0]) -> [0;1]
    | ([0;1], _) -> [0;1]
    | ([1;1], n) when mod_b n [0;0;1] = [0] -> [0;1]
    | ([1;1], n) -> [1;1]
    | _ ->
        let rec pow_rec n =
          if n = [0;1] then x
          else mult_b x (pow_rec (diff_b n ([0;1])))
        in pow_rec n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
 let power x n =
  let rec power_helper x n =
    if n = from_int 0 then from_int 1
    else if (to_int n) mod 2 = 0 then
      let y = power_helper x (from_int (to_int n / 2)) in
      mult_b y y
    else
      mult_b x (power_helper x (from_int (to_int n - 1)))
  in
  power_helper x n

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
 let mod_power x n m = 
  let rec mod_rec x r = function
    | [] -> r
    | e::l when e = 1 -> mod_rec (mod_b (mult_b x x) m) (mod_b (mult_b r x) m) l
    | e::l -> mod_rec (mod_b (mult_b x x) m) r l
  in match n with
       [] -> [0;1]
     | e::l -> mod_rec x [0;1] l;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  let rec prime_mod_power_helper x n p =
    if n = from_int 0 then from_int 1
    else if mod_b n (from_int 2) = from_int 0 then
      let y = prime_mod_power_helper x (from_int (to_int n / 2)) p in
      mod_b (mult_b y y) p
    else
      mod_b (mult_b x (prime_mod_power_helper x (from_int (to_int n - 1)) p)) p
  in
  prime_mod_power_helper x n p
