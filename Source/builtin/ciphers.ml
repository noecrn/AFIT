(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
 let encrypt_cesar k m b =
    let rec aux acc i =
      if i = List.length m then acc
      else
        let c = List.nth m i in
        let c' = modulo (c + k) b in
        aux (acc @ [c']) (i + 1)
    in
    aux [] 0 ;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
    let rec aux acc i =
        if i = List.length m then acc
        else
          let c = List.nth m i in
          let c' = modulo (c - k) b in
          aux (acc @ [c']) (i + 1)
      in
      aux [] 0 ;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let phi = (p - 1)*(q - 1)
    in let (u,v,pg) = (bezout (phi - 1) phi)
    in (p * q , phi - 1), (p * q, modulo u phi)

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
 let encrypt_rsa m (n, e) = prime_mod_power m e n

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
 let decrypt_rsa m (n , d) = prime_mod_power m d n

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
 let rec public_data_g p =
    let p = 2 * p + 1 in
    let rec find_g g =
      if modulo (p - 1) g = 0 then g
      else find_g (g + 1)
    in
    let g = find_g 2 in
    (g, p)

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = Random.int (p-1) in
  ((prime_mod_power g a p), a)

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
 let encrypt_g msg (g, p) kA =
    let k = Random.int (p-1) in
    let y = (prime_mod_power g k p)
    and x = (modulo (msg*(prime_mod_power kA k p)) p) in
    (y, x)

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
 let decrypt_g (msgA, msgB) a (g, p) =
  let y = prime_mod_power msgA a p in
  let (b, c, d) = bezout y p
 in modulo (msgB * b) p