(** Test suites for builtin basic_arithmetic ml file using alcotest. *)

open Alcotest
open Test_tools
open Scalable

let sprintf = Printf.sprintf

let from_int_tests () =
    let cases =
        [(32, [0; 0; 0; 0; 0; 0; 1]); (-3, [1; 1; 1]); (7, [0; 1; 1; 1])]
    and do_check (intgr , expected) =
        check
            (list int)
            (sprintf "from_int: %i" intgr)
            expected
            (from_int intgr)
    in
    List.iter do_check cases

let to_int_tests () =
    let cases =
        [([0; 0; 0; 0; 0; 0; 1], 32); ([1; 1; 1], -3); ([0; 1; 1; 1], 7)]
    and do_check (bitarray , expected) =
        check
            (int)
            (sprintf "to_int: %s" (string_of_intlist bitarray))
            expected
            (to_int bitarray)
    in
    List.iter do_check cases

let bigger_tests () =
    let cases =
        [([0;0;1;0;0;1], [0;0;1;1;0;1]), false;
         ([0;0;0;1;0;1], [0;1;0;0;0;1]), true;
         ([0;1;0;0;0;1], [0;1;0;0;0;1]), false;
         ([1;0;0;1;0;1], [1;1;1;0;0;1]), false;
         ([1;0;1;0;0;1],[1;1;1;0;0;1]), true]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "bigger_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a >> b)
    in
    List.iter do_check cases

let smaller_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), true;
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), false;
     ([0;1;0;0;0;1], [0;1;0;0;0;1]), false;
     ([1;0;0;1;0;1], [1;1;1;0;0;1]), true;
     ([1;0;1;0;0;1],[1;1;1;0;0;1]), false]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "smaller_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a << b)
    in
    List.iter do_check cases


let biggerEqual_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), false;
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), true;
     ([0;1;0;0;0;1], [0;1;0;0;0;1]), true;
     ([1;0;0;1;0;1], [1;1;1;0;0;1]), false;
     ([1;0;1;0;0;1],[1;1;1;0;0;1]), true]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "biggerEqual_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a >>= b)
    in
    List.iter do_check cases

let smallerEqual_tests () =
    let cases =
      [([0;0;1;0;0;1], [0;0;1;1;0;1]), true;
       ([0;0;0;1;0;1], [0;1;0;0;0;1]), false;
       ([0;1;0;0;0;1], [0;1;0;0;0;1]), true;
       ([1;0;0;1;0;1], [1;1;1;0;0;1]), true;
       ([1;0;1;0;0;1],[1;1;1;0;0;1]), false]
    and do_check ((a, b), expected) =
        check
            (bool)
            (sprintf "smallerEqual_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (a <<= b)
    in
    List.iter do_check cases

let add_b_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), [0;0;0;0;1;0;1];
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), [0;1;0;1;0;0;1];
     ([0;1;0;0;0;1], []), [0;1;0;0;0;1];
     ([1;0;0;1;0;1], [0;0;0;1;0;1]), []]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "add_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (add_b a b)
    in
    List.iter do_check cases

let diff_b_tests () =
  let cases =
    [([0;0;1;0;0;1], [0;0;1;1;0;1]), [1;0;0;1];
     ([0;0;0;1;0;1], [0;1;0;0;0;1]), [0;1;1];
     ([0;1;0;0;0;1], []), [0;1;0;0;0;1];
     ([1;0;0;1;0;1], [1;0;0;1;0;1]), []]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "diff_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (diff_b a b)
    in
    List.iter do_check cases

let mult_b_tests () =
  let cases =
    [([0;1], [0;0;1;1;0;1]), [0;0;1;1;0;1];
     ([1;1], [0;0;1;1;0;1]), [1;0;1;1;0;1];
     ([], [0;0;1;1;0;1]), [];
     ([0;0;1], [0;1;0;0;0;1]), [0;0;1;0;0;0;1];
     ([0;1;1], [0;1;1]), [0;1;0;0;1]]
    and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "mult_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (mult_b a b)
    in
    List.iter do_check cases

let mod_b_tests () =
  let cases =
    [([0;1], [0;0;1;1;0;1]), [0;1];
     ([0;1;0;1;0;1], [0; 0; 1]), [0;1];
     ([0;0;1;1;0;1], [0; 0; 1]), [];
     ([0;1;1;0;0;1], [0; 0; 0; 1]), [0; 1; 1]]
  and do_check ((a, b), expected) =
        check
            (list int)
            (sprintf "mod_b: %s and %s" (string_of_intlist a) (string_of_intlist b))
            expected
            (mod_b a b)
    in
    List.iter do_check cases

(****************************************************************************)
(****************************************************************************)

let scalable_set =
    [("Casting to bitarray function", `Quick, from_int_tests);
     ("Casting to int function", `Quick, to_int_tests);
     ("Bigger bitarray function", `Quick, bigger_tests);
     ("Smaller bitarray function", `Quick, smaller_tests);
     ("Bigger or Equal bitarray function", `Quick, biggerEqual_tests);
     ("Bigger or Equal bitarray function", `Quick, smallerEqual_tests);
     ("add_b bitarray function", `Quick, add_b_tests);
     ("diff_b or Equal bitarray function", `Quick, diff_b_tests);
     ("mult_b or Equal bitarray function", `Quick, mult_b_tests);
     ("mod_b or Equal bitarray function", `Quick, mod_b_tests);
    ]
