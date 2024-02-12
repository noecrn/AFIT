(** Factoring Builtin int primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break key =
    let n = fst key in
    let rec find_factors i =
      if i >= n then
        (0, 0)
      else if n mod i = 0 then
        (i, n / i)
      else
        find_factors (i + 1)
    in find_factors 2