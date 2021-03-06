(* Rope: an implementation of the data structure described in

   Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
   strings.  Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

   Motivated by Luca de Alfaro's extensible array implementation Vec.

   Copyright (C) 2007   Mauricio Fernandez <mfp@acm.org>
                        http://eigenclass.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the following special exception:

   You may link, statically or dynamically, a "work that uses the
   Library" with a publicly distributed version of the Library to
   produce an executable file containing portions of the Library, and
   distribute that executable file under terms of your choice, without
   any of the additional requirements listed in clause 6 of the GNU
   Lesser General Public License.  By "a publicly distributed version
   of the Library", we mean either the unmodified Library as
   distributed by the author, or a modified version of the Library that is
   distributed under the conditions defined in clause 2 of the GNU
   Lesser General Public License.  This exception does not however
   invalidate any other reasons why the executable file might be
   covered by the GNU Lesser General Public License.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   The GNU Lesser General Public License is available at
   http://www.gnu.org/copyleft/lgpl.html; to obtain it, you can also
   write to the Free Software Foundation, Inc., 51 Franklin St,
   Fifth Floor, Boston, MA 02110-1301, USA.
 *)

(* =begin ignore *)
type t =
    Empty
  (* left, left size, right, right size, height *)
  | Concat of t * int * t * int * int
  | Leaf of string


let height = function
    Empty | Leaf _ -> 0
  | Concat(_,_,_,_,h) -> h

(* For debugging and judging balancing *)
let print =
  let rec print prefix = function
    | Empty -> print_string "\n"
    | Leaf s ->
        print_string prefix;
        print_string s;
        print_string "\n"
    | Concat(l,_, r,_, _) ->
        let prefixl = prefix ^ if height l = 0 then "/" else " " in
        let prefixr = prefix ^ if height r = 0 then "\\" else " " in
        print prefixl l;
        print prefixr r;
  in print ""


type forest_element = { mutable c : t; mutable len : int }

let str_append = (^)
let string_of_string_list l = String.concat "" l

module STRING = String

(* 48 limits max rope size to 220GB on 64 bit,
 * ~ 700MB on 32bit (length fields overflow after that) *)
let max_height = 48

(* actual size will be that plus 1 word header;
 * the code assumes it's an even num.
 * 256 gives up to a 50% overhead in the worst case (all leaf nodes near
 * half-filled *)
let leaf_size = 256
(* let leaf_size = 128 *)
(* let leaf_size = 64 *)
(* let leaf_size = 32 *)

(* =end *)

(* =begin code *)

exception Out_of_bounds

let empty = Empty

(* by construction, there cannot be Empty or Leaf "" leaves *)
let is_empty = function Empty -> true | _ -> false


let length = function
    Empty -> 0
  | Leaf s -> STRING.length s
  | Concat(_,cl,_,cr,_) -> cl + cr

let make_concat l r =
  let hl = height l and hr = height r in
  let cl = length l and cr = length r in
    Concat(l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)

let min_len =
  let fib_tbl = Array.make max_height 0 in
  let rec fib n = match fib_tbl.(n) with
      0 ->
        let last = fib (n - 1) and prev = fib (n - 2) in
        let r = last + prev in
        let r = if r > last then r else last in (* check overflow *)
          fib_tbl.(n) <- r; r
    | n -> n
  in
    fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
    Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))

let max_length = min_len.(Array.length min_len - 1)

let concat_fast l r = match l with
    Empty -> r
  | Leaf _ | Concat(_,_,_,_,_) ->
      match r with
          Empty -> l
        | Leaf _ | Concat(_,_,_,_,_) -> make_concat l r

(* based on Hans-J. Boehm's *)
let add_forest forest rope len =
  let i = ref 0 in
  let sum = ref empty in
  while len > min_len.(!i+1) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      forest.(!i).c <- Empty
    end;
    incr i
  done;
  sum := concat_fast !sum rope;
  let sum_len = ref (length !sum) in
  while !sum_len >= min_len.(!i) do
    if forest.(!i).c <> Empty then begin
      sum := concat_fast forest.(!i).c !sum;
      sum_len := !sum_len + forest.(!i).len;
      forest.(!i).c <- Empty;
    end;
    incr i
  done;
  decr i;
  forest.(!i).c <- !sum;
  forest.(!i).len <- !sum_len

let concat_forest forest =
  Array.fold_left (fun s x -> concat_fast x.c s) Empty forest

let rec balance_insert rope len forest = match rope with
    Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat(l,cl,r,cr,h) when h >= max_height || len < min_len.(h) ->
      balance_insert l cl forest;
      balance_insert r cr forest
  | x -> add_forest forest x len (* function or balanced *)

let balance r =
  match r with
      Empty -> Empty
    | Leaf _ -> r
    | _ ->
        let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
          balance_insert r (length r) forest;
          concat_forest forest

let bal_if_needed l r =
  let r = make_concat l r in
    if height r < max_height then r else balance r

let concat_str l = function
    Empty | Concat(_,_,_,_,_) -> invalid_arg "concat_str"
  | Leaf rs as r ->
      let lenr = STRING.length rs in
        match l with
          | Empty -> r
          | Leaf ls ->
              let slen = lenr + STRING.length ls in
                if slen <= leaf_size then Leaf (str_append ls rs)
                else make_concat l r (* height = 1 *)
          | Concat(ll, cll, Leaf lrs, clr, h) ->
              let slen = clr + lenr in
                if clr + lenr <= leaf_size then
                  Concat(ll, cll, Leaf (str_append lrs rs), slen, h)
                else
                  bal_if_needed l r
          | _ -> bal_if_needed l r

let append_char c r = concat_str r (Leaf (STRING.make 1 c))

let concat l = function
    Empty -> l
  | Leaf _ as r -> concat_str l r
  | Concat(Leaf rls,rlc,rr,rc,h) as r ->
      (match l with
          Empty -> r
        | Concat(_,_,_,_,_) -> bal_if_needed l r
        | Leaf ls ->
            let slen = rlc + STRING.length ls in
              if slen <= leaf_size then
                Concat(Leaf(str_append ls rls), slen, rr, rc, h)
              else
                bal_if_needed l r)
  | r -> (match l with Empty -> r | _ -> bal_if_needed l r)

let prepend_char c r = concat (Leaf (STRING.make 1 c)) r

let rec get i = function
    Empty -> raise Out_of_bounds
  | Leaf s ->
      if i >= 0 && i < STRING.length s then STRING.unsafe_get s i
      else raise Out_of_bounds
  | Concat (l, cl, r, _, _) ->
      if i < cl then get i l
      else get (i - cl) r

let rec getn i = function
    Empty -> raise Out_of_bounds
  | Leaf _ ->
      0
  | Concat (l, cl, r, _, _) ->
      if i < cl then 1 + getn i l else 1 + getn (i - cl) r


let rec set i v = function
    Empty -> raise Out_of_bounds
  | Leaf s ->
      if i >= 0 && i < STRING.length s then
        let s = Bytes.of_string s in
        Bytes.unsafe_set s i v;
        Leaf (Bytes.unsafe_to_string s)
      else raise Out_of_bounds
  | Concat(l, cl, r, _, _) ->
      if i < cl then concat (set i v l) r
      else concat l (set (i - cl) v r)

let of_string = function
    s when STRING.length s = 0 -> Empty
  | s ->
      let min (x:int) (y:int) = if x <= y then x else y in
      let rec loop r s len i =
        if i < len then (* len - i > 0, thus Leaf "" can't happen *)
          loop (concat r (Leaf (STRING.sub s i (min (len - i) leaf_size))))
            s len (i + leaf_size)
        else
          r
      in loop Empty s (STRING.length s) 0

let rec make len c =
  let rec concatloop len i r =
    if i <= len then
      concatloop len (i * 2) (concat r r)
    else r
  in
    if len = 0 then Empty
    else if len <= leaf_size then Leaf (STRING.make len c)
    else
      let rope = concatloop len 2 (of_string (STRING.make 1 c)) in
        concat rope (make (len - length rope) c)

let rec sub start len = function
    Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds else Empty
  | Leaf s ->
      if len > 0 then (* Leaf "" cannot happen *)
        (try Leaf (STRING.sub s start len) with _ -> raise Out_of_bounds)
      else if len < 0 || start < 0 || start > STRING.length s then
        raise Out_of_bounds
      else Empty
  | Concat(l,cl,r,cr,_) ->
      if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
      let left =
        if start = 0 then
          if len >= cl then
            l
          else sub 0 len l
        else if start > cl then Empty
        else if start + len >= cl then
          sub start (cl - start) l
        else sub start len l in
      let right =
        if start <= cl then
          let upto = start + len in
            if upto = cl + cr then r
            else if upto < cl then Empty
            else sub 0 (upto - cl) r
        else sub (start - cl) len r
      in
        concat left right

let insert start rope r =
  concat (concat (sub 0 start r) rope) (sub start (length r - start) r)

let remove start len r =
  concat (sub 0 start r) (sub (start + len) (length r - start - len) r)

let to_string r =
  let rec strings l = function
      Empty -> l
    | Leaf s -> s :: l
    | Concat(left,_,right,_,_) -> strings (strings l right) left
  in
    string_of_string_list (strings [] r)

let rec iter f = function
    Empty -> ()
  | Leaf s -> STRING.iter f s
  | Concat(l,_,r,_,_) -> iter f l; iter f r

let iteri f r =
  let rec aux f i = function
    Empty -> ()
  | Leaf s ->
      for j = 0 to STRING.length s - 1 do
        f (i + j) (STRING.unsafe_get s j)
      done
  | Concat(l,cl,r,_,_) -> aux f i l; aux f (i + cl) r
  in
    aux f 0 r

let rec rangeiter f start len = function
    Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
  | Leaf s ->
      let n = start + len in
      let lens = STRING.length s in
      if start >= 0 && len >= 0 && n <= lens then
        for i = start to n - 1 do
          f (STRING.unsafe_get s i)
        done
      else raise Out_of_bounds
  | Concat(l,cl,r,cr,_) ->
      if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
      if start < cl then begin
        let upto = start + len in
          if upto <= cl then
            rangeiter f start len l
          else begin
            rangeiter f start (cl - start) l;
            rangeiter f 0 (upto - cl) r
          end
      end else begin
        rangeiter f (start - cl) len r
      end

let rec fold f a = function
    Empty -> a
  | Leaf s ->
      let acc = ref a in
        for i = 0 to STRING.length s - 1 do
          acc := f !acc (STRING.unsafe_get s i)
        done;
        !acc
  | Concat(l,_,r,_,_) -> fold f (fold f a l) r

(* =end *)
