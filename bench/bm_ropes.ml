(* File: bm_ropes.ml

   Copyright (C) 2007

     Christophe Troestler <Christophe.Troestler@umh.ac.be>
     WWW: http://math.umh.ac.be/an/software/

     Mauricio Fernandez <mfp@acm.org>
     http://eigenclass.org

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Printf
let () = Random.self_init()

let nsamples = 30
let n_ops = 1000
(* let n_ops = 10000 *)

let max_nconcat = 8
  (** how many concatenations of ropes (in the map) with lower length
      are accepted. [1] means: pick the laegest and complete appending
      chars. *)

let prepend_size = 128
  (** if [size land prepend_size = 0] prepend chars instead of appending.
      Set to [max_int]: always append (never prepend).
      Set to [0]: always preprend. *)

let min_pow10 = 2
let max_pox10 = 7

let rec pow n = function 0 -> 1 | i -> n * pow n (i - 1)
let list_init n f =
  let rec make acc n = if n < 0 then acc else make (f n :: acc) (n - 1) in
  make [] (n-1)

let datapoints =
  let basis = [1; 2; 3; 5; 17; 37; 53; 91; 201] in
(*   let basis = List.sort compare (list_init 15 (fun _ -> 1 + Random.int 200)) in *)
  let d = max_pox10 - min_pow10 in
  let pow10_of j = Array.init d (fun i -> j * pow 10 (i + min_pow10)) in
  Array.concat (List.map pow10_of basis (*@ [ [|max_int / 2 |] } *) )
(* FIXME: for max_int (32 bits), TinyRope segfaults!!! *)

let datapoints2 =
  Array.concat [
    (Array.init 20 (fun _ -> 10000 + Random.int 10_000_000));
    (Array.init 20 (fun _ -> 10_000_000 + Random.int 50_000_000));
  ]


(* ---------------------------------------------------------------------- *)

let datapoints_ordered =
  let d = Array.copy datapoints in
  Array.sort compare d;
  d

(* just for laughs  *)
let basic_loop_overhead =
  let t1 = Unix.gettimeofday () in
    for j = 0 to 100 do
      for i = 0 to n_ops do ignore () done
    done;
    (Unix.gettimeofday () -. t1) /. 100.0

let random_loop_overhead =
  let t1 = Unix.gettimeofday () in
    for j = 0 to 100 do
      for i = 0 to n_ops do ignore (Random.int 10000) done;
    done;
    (Unix.gettimeofday () -. t1) /. 100.0

let () =
  printf "Random loop overhead: %12.10f\n" random_loop_overhead;
  printf "Basic loop overhead: %12.10f\n" basic_loop_overhead

let time ~msg f x =
  let t0 = Sys.time () in
  let r = f x in
    printf "%s needed %8.5fs\n%!" msg (Sys.time () -. t0);
    r

let sample msg f x =
  print_string msg;
  let samples =
    Array.init nsamples (fun i -> printf "\r%2d/%4d%!" (i + 1) nsamples; f x) in
  printf "\r               \r%!";
  let min_sample (tmin,_) (t,d) = (min tmin t, d) in
  Array.fold_left min_sample (max_float, max_int) samples
(*  let sum_sample (tsum, _) (t,d) = (tsum +. t, d) in
  let t, d = Array.fold_left sum_sample (0.0, 0) samples in
  t /. float_of_int nsamples, d
*)

module IMap = Map.Make(struct type t = int let compare = compare end)

module Benchmark(R :
  sig
    type t
    val name : string
    val balanced : bool
    val empty : t
    val append : char -> t -> t
    val prepend : char -> t -> t
    val concat : t -> t -> t
    val length : t -> int
    val height : t -> int
    val balance : t -> t
    val get : t -> int -> char
    val sub : t -> int -> int -> t
    val of_string : string -> t
    val to_string : t -> string
  end) =
struct
  (** [make_rope size] returns a rope of length [size].  We
      concatenate small ropes as it is more reprensentative than only
      appending repeatedly chars. *)
  let make_rope =
    let rope_tbl = ref IMap.empty in
    let rec add_chars r c size =
      if size <= 0 then r else
        let op = if size land prepend_size = 0 then R.prepend else R.append in
        add_chars (op c r) c (size - 1)  in
    let rec build nconcat r size =
      let largest =
        IMap.fold (fun _ v s ->
          let len = R.length v in
          if len > R.length s && len <= size then v else s
        ) !rope_tbl R.empty in
      if R.length largest = 0 || nconcat > max_nconcat then
        (* no piece to add to [r] *)
        add_chars r 'x' (size - R.length largest)
      else
        let r' =
          if Random.bool() then R.concat r largest else R.concat largest r in
        build (nconcat + 1) r' (size - R.length largest) in
    fun size ->
      let r = build 0 R.empty size in
      rope_tbl := IMap.add size r !rope_tbl;
      if R.balanced then R.balance r else r
  ;;

  let append_time size =
    let v = ref (make_rope size) in
    let t0 = Unix.gettimeofday () in
    for i = 0 to n_ops - 1 do
      v := R.append 'z' !v;
      (* ignore (append_f I !v); *)
    done;
    let dt = (Unix.gettimeofday () -. t0) in
    (dt -. basic_loop_overhead) /. (float_of_int n_ops),  R.height !v

  let measure_append_time size =
    let msg = sprintf "Append time for %s of size %d\n%!" R.name size in
    sample msg append_time size


  let random_get_time size =
    let r = make_rope size in
(*     Gc.full_major (); *)
    let t0 = Unix.gettimeofday () in
(*     let sum = ref 0 in *)
    for i = 0 to n_ops - 1 do
      ignore(R.get r (Random.int size));
    done;
    let dt = (Unix.gettimeofday () -. t0) in
    (dt -. random_loop_overhead) /. float n_ops,  R.height r
(*     float !sum /. float n_ops,  R.height r *)

  let measure_random_get_time size =
    let msg = sprintf "Random get time for %s of size %d\n%!" R.name size in
    sample msg random_get_time size


  let sub_time size =
    let r = make_rope size in
    let t0 = Unix.gettimeofday () in
    let h = ref 0 in
    for i = 0 to n_ops - 1 do
      h := !h + R.height(R.sub r 0 (Random.int size));
    done;
    let dt = (Unix.gettimeofday () -. t0) in
    (dt -. random_loop_overhead) /. float n_ops,
    truncate(0.5 +. float !h /. float n_ops) (* round *)

  let measure_sub_time size =
    let msg = sprintf "Sub time for %s of size %d\n%!" R.name size in
    sample msg sub_time size

  (* Test inspired by http://www.rubyquiz.com/quiz137.html *)
  let size = 512 * 1024
  let size8 = 8 + size
  (* [text] is make of [nchunks] chunks of text, each of [size] bytes
     long.  Each chunck starts with an 8 byte number.  Initially the
     chuncks are shuffled the this function sorts them into ascending
     order. *)
  let rec qsort text =
    if R.length text = 0 then text else begin
      let pivot = int_of_string(R.to_string(R.sub text 0 8)) in
      let less = ref R.empty
      and more = ref R.empty in
      let offset = ref size8 in
      while !offset < R.length text do
        let i = int_of_string(R.to_string(R.sub text !offset 8)) in
        if i < pivot then
          less := R.concat !less (R.sub text !offset size8)
        else
          more := R.concat !more (R.sub text !offset size8);
        offset := !offset + 8 + size;
      done;
      R.concat (qsort !less) (R.concat (R.sub text 0 size8) (qsort !more))
    end

  let bulk_string = make_rope size
  let do_qsort size =
    let nchunks = size / 100_000 in
    let data = ref R.empty in
    for i = 1 to nchunks do
      data := R.concat !data
        (R.concat (R.of_string(sprintf "%08i" (Random.int nchunks)))
           bulk_string)
    done;
    let t0 = Unix.gettimeofday () in
    let sorted = qsort !data in
    let dt = (Unix.gettimeofday () -. t0) in
    (dt -. random_loop_overhead) /. float n_ops,  R.height sorted

  let measure_qsort size =
    let msg = sprintf "Qsort time for %s of size %d\n%!" R.name size in
    sample msg do_qsort size
end

module TinyBM =
struct
  let name = "TinyRope"
  include TinyRope
  let append = TinyRope.append_char
  let prepend = TinyRope.prepend_char
  let get r i = TinyRope.get i r
  let sub r start len = TinyRope.sub start len r
end

module FullBM =
struct
  let name = "Rope"
  include Rope
  let append c r = Rope.concat2 r (Rope.of_char c)
  let prepend c r = Rope.concat2 (Rope.of_char c) r
  let concat = Rope.concat2
end

module BalancedFullBM =
  Benchmark(struct include FullBM let balanced = true end)
module UnbalancedFullBM =
  Benchmark(struct include FullBM let balanced = false end)
module BalancedTinyBM =
  Benchmark(struct include TinyBM let balanced = true end)
module UnbalancedTinyBM =
  Benchmark(struct include TinyBM let balanced = false end)

let benchmark dst measl =
  let gather_times f =
    Array.fold_left (fun bm size -> IMap.add size (f size) bm)
      IMap.empty datapoints in
  let times = List.map gather_times measl in
  let ch = open_out (Filename.concat "bench" dst) in
  Array.iter (fun size ->
    fprintf ch "%d" size;
    List.iter (fun tbl ->
      let t, sz = IMap.find size tbl in
      fprintf ch "\t%12.10e\t%i" t sz
    ) times;
    fprintf ch "\n"
  ) datapoints_ordered;
  close_out ch

let () =
  benchmark "append.dat" [UnbalancedTinyBM.measure_append_time;
                          UnbalancedFullBM.measure_append_time ];
  Gc.full_major ();
  benchmark "get.dat" [UnbalancedTinyBM.measure_random_get_time;
                       UnbalancedFullBM.measure_random_get_time ];
  Gc.full_major ();
  benchmark "append-balanced.dat" [BalancedTinyBM.measure_append_time;
                                   BalancedFullBM.measure_append_time];
  Gc.full_major ();
  benchmark "get-balanced.dat" [BalancedTinyBM.measure_random_get_time;
                                BalancedFullBM.measure_random_get_time];
  Gc.full_major ();
  benchmark "sub.dat" [UnbalancedTinyBM.measure_sub_time;
                       UnbalancedFullBM.measure_sub_time;
                       BalancedTinyBM.measure_sub_time;
                       BalancedFullBM.measure_sub_time ];
  Gc.full_major ();
  benchmark "qsort.dat" [UnbalancedTinyBM.measure_qsort;
                         UnbalancedFullBM.measure_qsort;
                         BalancedTinyBM.measure_qsort;
                         BalancedFullBM.measure_qsort ];
  ()

