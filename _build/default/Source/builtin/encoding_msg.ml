(** Encoding Strings *)

open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
    let rec encode_rec str acc =
      match str with
      | "" -> acc
      | s ->
          let c = Char.code (String.get s 0) in
          let acc' = acc * (power 2 bits) + c in
          encode_rec (String.sub s 1 (String.length s - 1)) acc'
    in
    encode_rec str 0
  

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let ascii_table =
    let table = Array.make 128 "" in
    for i = 0 to 127 do
      table.(i) <- String.make 1 (Char.chr i)
    done;
    table
  in

  let rec decode_rec acc msg =
    if msg = 0 then acc
    else
      let c = msg mod (power 2 bits) in
      let acc' = (ascii_table.(c)) ^ acc in
      decode_rec acc' (msg / (power 2 bits))
  in
  decode_rec "" msg
