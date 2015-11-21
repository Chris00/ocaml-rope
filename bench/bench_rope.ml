open Benchmark

let empty = Rope.empty
let ( ^^ ) = Rope.concat2

let rec random_string len =
  let s = Bytes.create len in
  for i = 0 to len - 1 do Bytes.set s i (Char.chr(Random.int 255)) done;
  Bytes.unsafe_to_string s

(* Creates the list [[f 0;...; f(len - 1)]] *)
let list_init len f =
  let rec build acc i =
    if i >= 0 then build (f i :: acc) (i - 1) else acc in
  build [] (len - 1)

(* ---------------------------------------- *)

let string r sub =
  let r = List.fold_left ( ^ ) "" r in
  let r = List.fold_left (fun sum (i,l) -> sum ^ String.sub r i l) "" sub in
  for i = 0 to String.length r - 1 do
    ignore(String.get r i)
  done

let rope r sub =
  let r = List.fold_left ( ^^ ) empty r in
  let r = List.fold_left (fun sum (i,l) -> sum ^^ Rope.sub r i l) empty sub in
  for i = 0 to Rope.length r - 1 do
    ignore(Rope.get r i)
  done

let iter r sub =
  let r = List.fold_left ( ^^ ) empty r in
  let r = List.fold_left (fun sum (i,l) -> sum ^^ Rope.sub r i l) empty sub in
  let i = Rope.Iterator.make r 0 in
  try
    while true do ignore(Rope.Iterator.get i); Rope.Iterator.incr i done
  with Rope.Out_of_bounds _ -> ()


let () =
  Random.self_init();
  let init_len = 300 (* size of initial strings *) in
  let init_num = 10 (* howmany initial strings *) in
  let sub = list_init 0 (fun _ ->
    let i = Random.int init_len in (i, Random.int (init_len - i))) in
  let s = list_init init_num (fun _ -> random_string init_len) in
  let r = List.map Rope.of_string s in
  let res = throughputN 5 ~repeat:3
    [("String", string s, sub);
     ("Rope", rope r, sub);
     ("Iter", iter r, sub)  ] in
  tabulate res
