(** Basic arithmetics with builtin integers *)

open Builtin

(* #use "Builtin.ml" *)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
*)

let rec gcd a b =
  if a < 0 then gcd (a * (-1)) b
  else 
    if b < 0 then gcd a (b * (-1))
    else
      if a < b then gcd b a 
      else
        if modulo a b == 0 then b
        else gcd b (modulo a b)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)

let bezout a b = 
    let rec euc(a,xa,ya,b,xb,yb) =
        if yb = 0 then
          (a,xa,ya)
        else
          let q = ya/yb in
          euc (b,xb,yb,a - q * b, xa - q * xb, ya - q * yb)
      in euc (1,0,a,0,1,b) ;;