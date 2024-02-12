(** Chinese remainder theorem *)

open Builtin
open Basic_arithmetics

(** Image of the Chinese Remainder map
    @param x positive integer of which you take image
    @param l list of pairwise relatively prime positive integers.
 *)
let crt_image x l =
  let rec helper x l acc =
    match l with
    | [] -> acc
    | (n, y) :: t -> helper x t (acc @ [(x mod n, n)])
  in helper x l []

(** Inverse image of Chinese Remainder map
    @param a positive integer
    @param l list of pairwise relatively prime factors of m
    @param y list of remainders modulo pairwise relatively prime factors of m
 *)
let crt_solver m l y =
  let rec helper m l y acc =
    match l with
    | [] -> acc
    | (n, x) :: t ->
      let (u, v, d) = bezout n m in
        let (s, t) =
          if d = 1 then
          (u, (x - acc) / n)
          else
          raise (Invalid_argument "not pairwise relatively prime")
        in helper m t y (acc + s * t * n)
      in helper m l y 0
    