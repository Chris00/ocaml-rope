(* Simple tests to be performed in the toploop. *)

#use "rope_top.ml";;
let ( ^ ) = Rope.concat2
let rope = Rope.of_string

let pow r i =
  let rec pow_rec m r i =
    if i = 0 then m
    else if i mod 2 = 0 then pow_rec m (r ^ r) (i/2)
    else (* odd i *) pow_rec (m ^ r) (r ^ r) (i/2) in
  pow_rec Rope.empty r i
;;

let r = rope "Hello" ^ rope " " ^ rope "world!";;
r ^ Rope.empty;;
Rope.length r = 12;;

let r = rope "Hello world! "
let n = 50_000_000
let rn = pow r n;;
Rope.length rn = n * Rope.length r;;
Rope.get rn (10 * Rope.length r) = 'H';;

let r = "Hello world! ¡Hola amigos!" in
let n = String.length r in
Rope.sub (rope r) (n-2) 1;;

Rope.index Rope.empty 'c';;
Rope.index (rope "abcde") 'c' = 2;;
Rope.equal (Rope.sub (rope "Hélicoïdal") 5 3) (rope "oïd");;
Rope.compare (rope "Hello") (rope "Hello ");;
Rope.compare (rope "") (rope "Hello");;

let b = Rope.Buffer.create 1;;
Rope.Buffer.contents b;;
Rope.Buffer.add_string b "Hello ";;
Rope.Buffer.add_rope b (rope "world!");;
Rope.Buffer.contents b;;
