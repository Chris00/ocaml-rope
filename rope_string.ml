(* File: rope.ml

   Copyright (C) 2007

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

(** Rope implementation inspired from

    Hans Boehm, Russ Atkinson, Michael Plass, "Ropes: an alternative
    to strings", Software Practice and Experience 25, vol. 12 (1995),
    pp. 1315-1330.

    http://www.cs.ubc.ca/local/reading/proceedings/spe91-95/spe/vol25/issue12/spe986.pdf *)

(* TODO:
   - Camomille interop. (with phantom types for encoding ??)
 *)

module S = String

let min i j = if (i:int) < j then i else j
let max i j = if (i:int) > j then i else j

exception Out_of_bounds of string

(* One assumes throughout that the length is a representable
   integer.  Public functions that allow to construct larger ropes
   must check this. *)
type t =
  | String of string
      (* FIXME: (s, i0, len) where only s.[i0 .. i0+len-1] is used by the
         rope.  [len = 0] is forbidden, unless the rope has 0
         length (i.e. it is empty). *)
  | Concat of int * int * t * int * t
      (* [(depth, length, left, left_length, right)].  This asymmetry
         between left and right was chosen because the full length and
         the left length are more often needed that the right
         length. *)

type rope = t

let concat_flatten_length = 32
  (* Ropes of length [<= concat_flatten_length] may be flattened by
     concat2.  [concat_flatten_length] must be quite small, typically
     comparable to the size of a Concat node. *)

let small_rope_length = 256
  (* Under this length, flat string are considered better. *)


(* Fibonacci numbers $F_{n+2}$.  By definition, a NON-EMPTY rope [r]
   is balanced iff [length r >= min_length.(depth r)].
   [max_depth] is the first depth at which the fib. number overflow
   the integer range. *)
let min_length, max_depth =
  (* Since F_{n+2} >= ((1 + sqrt 5)/2)^n, we know F_{d+2} will overflow: *)
  let d = (3 * Sys.word_size) / 2 in
  let m = Array.make d max_int in
  (* See [add_nonempty_to_forest] for the reason for [max_int] *)
  let prev = ref 0
  and last = ref 1
  and i = ref 0 in
  try
    while !i < d - 1 do
      let curr = !last + !prev in
      if curr < !last (* overflow *) then raise Exit;
      m.(!i) <- curr;
      prev := !last;
      last := curr;
      incr i
    done;
    assert false
  with Exit -> m, !i

let depth_to_rebalance = max_depth
  (* Beyond this depth, implicit balance will be done.  This value
     allows gross inefficiencies while not being too time consuming.
     For example, explicit rebalancing did not improve the running
     time on the ICFP 2007 task. *)

let empty = String ""

let length = function
  | String s -> S.length s
  | Concat(_,len,_,_,_) -> len

let depth = function
  | String _ -> 0
  | Concat(d,_,_,_,_) -> d

let is_empty = function
  | String s -> s = ""
  | _ -> false

let is_not_empty = function
  | String s -> s <> ""
  | _ -> true

let unsafe_of_string s = String s
  (* Not for general consumption.  Can be useful for literal strings
     in a source (thus for some camlp4 automatic transformation).
     FIXME: Maybe we do not even want to provide this as it allows
     to construct big leaves. *)

(* Since we will need to copy the string anyway, let us take this
   opportunity to split it in small chunks for easier further
   sharing.  In order to minimize the depth, we use a simple
   bisection scheme. *)
let rec unsafe_of_substring s i len =
  if len <= small_rope_length then String(S.sub s i len)
  else
    let len' = len / 2 in
    let i' = i + len' in
    let left = unsafe_of_substring s i len'
    and right = unsafe_of_substring s i' (len - len') in
    let d = 1 + max (depth left) (depth right) in
    let ll = length left in
    Concat(d, ll + length right, left, ll, right)

let of_substring s i len =
  if i < 0 || len < 0 || i > S.length s - len then
    invalid_arg "Rope.of_substring";
  unsafe_of_substring s i len

let of_string s = unsafe_of_substring s 0 (String.length s)

let of_char c = String(String.make 1 c)

(* [copy_to_substring t ofs r] copy the rope [r] to the substring
   [t.[ofs .. ofs+(length r)-1]].  It is assumed that [t] is long enough.
   (This function could be a one liner with [iteri] but we want to use
   [S.blit] for efficiency.) *)
let rec copy_to_substring t ofs = function
  | String s ->
      assert(ofs >= 0 && ofs <= S.length t - String.length s);
      S.unsafe_blit s 0 t ofs (String.length s)
  | Concat(_, _, l,ll, r) ->
      copy_to_substring t ofs l;
      copy_to_substring t (ofs + ll) r

let to_string r =
  let len = length r in
  if len > Sys.max_string_length then failwith "Rope.string_of_rope";
  let t = String.create len in
  copy_to_substring t 0 r;
  t

let rec get rope i = match rope with
  | String s ->
      if i < 0 || i >= String.length s then raise(Out_of_bounds "Rope.get")
      else s.[i]
  | Concat(_, _, l, left_len, r) ->
      if i < left_len then get l i else get r (i - left_len)

let rec iter f = function
  | String s -> for i = 0 to String.length s - 1 do f s.[i] done
  | Concat(_, _, l,_, r) -> iter f l; iter f r

let rec iteri_rec f init = function
  | String s ->
      for i = 0 to String.length s - 1 do f (i + init) s.[i] done
  | Concat(_, _, l,ll, r) ->
      iteri_rec f init l;
      iteri_rec f (init + ll) r

let iteri f r = ignore(iteri_rec f 0 r)

(* [balance_concat] is more unsafe than |concat2] (below) because it
   does not check for the length overflowing int range.  It is also
   more aggressive in flattening to reduce the depth *)
let balance_concat rope1 rope2 =
  let len1 = length rope1
  and len2 = length rope2 in
  let len = len1 + len2 in
  if len1 = 0 then rope2
  else if len2 = 0 then rope1
  else if len <= small_rope_length then (
    (* flatten the result *)
    let s = S.create len in
    copy_to_substring s 0 rope1;
    copy_to_substring s len1 rope2;
    String s
  )
  else match rope1, rope2 with
  | Concat(d1, _, l1,ll1, r1), _ when len1 - ll1 <= small_rope_length - len2 ->
      let rl1 = len1 - ll1 in
      let l = rl1 + len2 in
      let s = S.create l in
      copy_to_substring s 0 r1;
      copy_to_substring s rl1 rope2;
      Concat(d1, len, l1,ll1, String s)
  | _, Concat(d2, _, l2,ll2, r2) when ll2 <= small_rope_length - len1 ->
      let l = len1 + ll2 in
      let s = S.create l in
      copy_to_substring s 0 rope1;
      copy_to_substring s len1 l2;
      Concat(d2, len, String s, l, r2)
  | _, _ ->
      let d = 1 + max (depth rope1) (depth rope2) in
      Concat(d, len, rope1, len1, rope2)
;;


(* Invariants for [forest]:
   1) The concatenation of the forest (in decreasing order) with the
      unscanned part of the rope is equal to the rope being balanced.
   2) All trees in the forest are balanced, i.e. [forest.(n)] is empty or
      [length forest.(n) >= min_length.(n)].
   3) [depth forest.(n) <= n] *)
(* Add the rope [r] (usually a leaf) to the appropriate slot of
   [forest] (according to [length r]) gathering ropes from lower
   levels if necessary.  Assume [r] is not empty. *)
let add_nonempty_to_forest forest r =
  let len = length r in
  let n = ref 0 in
  let sum = ref empty in
  (* forest.(n-1) ^ ... ^ (forest.(2) ^ (forest.(1) ^ forest.(0)))
     with [n] s.t. [min_length.(n) < len <= min_length.(n+1)].  [n]
     is at most [max_depth-1] because [min_length.(max_depth) = max_int] *)
  while len > min_length.(!n + 1) do
    if is_not_empty forest.(!n) then (
      sum := balance_concat forest.(!n) !sum;
      forest.(!n) <- empty;
    );
    incr n
  done;
  (* Depth of [sum] at most 1 greater than what would be required
     for balance. *)
  sum := balance_concat !sum r;
  (* If [depth r <= !n - 1] (e.g. if [r] is a leaf), then [!sum] is
     now balanced -- distinguish whether forest.(!n - 1) is empty or
     not (see the cited paper pp. 1319-1320).  We now continue
     concatenating ropes until the result fits into an empty slot of
     the [forest]. *)
  let sum_len = ref(length !sum) in
  while !n < max_depth && !sum_len >= min_length.(!n) do
    if is_not_empty forest.(!n) then (
      sum := balance_concat forest.(!n) !sum;
      sum_len := length forest.(!n) + !sum_len;
      forest.(!n) <- empty;
    );
    incr n
  done;
  decr n;
  forest.(!n) <- !sum

let add_to_forest forest r =
  if is_not_empty r then add_nonempty_to_forest forest r

(* Add a NON-EMPTY rope [r] to the forest *)
let rec balance_insert forest rope = match rope with
  | String s ->
      (* If the length of the leaf is small w.r.t. the length of
         [s], extract it to avoid keeping a ref the larger [s].
         FIXME: good idea?*)
      add_nonempty_to_forest forest rope
  | Concat(d, len, l,_, r) ->
      (* FIXME: when to rebalance subtrees *)
      if d >= max_depth || len < min_length.(d) then (
        (* sub-rope needs rebalancing *)
        balance_insert forest l;
        balance_insert forest r;
      )
      else add_nonempty_to_forest forest rope

let concat_forest forest =
  Array.fold_left (fun sum r -> balance_concat r sum) empty forest

let balance = function
  | String _ as r -> r
  | r ->
      let forest = Array.make max_depth empty in
      balance_insert forest r;
      concat_forest forest

let balance_if_needed r =
  if depth r >= depth_to_rebalance then balance r else r
;;

(* "Fast" concat for ropes. *)
let rec concat2_nonempty rope1 rope2 =
  match rope1, rope2 with
  | String s1, String s2 ->
      let len1 = String.length s1 and len2 = String.length s2 in
      let len = len1 + len2 in
      if len <= concat_flatten_length then
        let s = S.create len in
        copy_to_substring s 0 rope1;
        copy_to_substring s len1 rope2;
        String s
      else
        Concat(1, len, rope1, len1, rope2)
  | Concat(d1, len1, l1,ll1, r1), _ ->
      let rl1 = len1 - ll1 in
      let len2 = length rope2 in
      if rl1 <= concat_flatten_length - len2 && depth r1 < d1 then
        let l = rl1 + len2 in
        let s = S.create l in
        copy_to_substring s 0 r1;
        copy_to_substring s rl1 rope2;
        Concat(d1, len1 + len2, l1,ll1, String s)
(*      else if depth l1 >= depth r1 + depth rope2 && depth r1 <= 3 then
        (* the depth is achieved by the left branch *)
        let r1_2 = concat2_nonempty r1 rope2 in
        if depth r1_2 <= depth l1 then
          let d = 1 + max (depth l1) (depth r1_2) in
          Concat(d, len1 + len2, l1, ll1, r1_2)
        else
          let d = 1 + max (depth rope1) (depth rope2) in
          Concat(d, len1 + len2, rope1, len1, rope2)
*)      else
        let d = 1 + max (depth rope1) (depth rope2) in
        Concat(d, len1 + len2, rope1, len1, rope2)
  | _, Concat(d2, len2, l2,ll2, r2) ->
      let len1 = length rope1 in
      if ll2 <= concat_flatten_length - len1 && depth l2 < d2 then
        let l = len1 + ll2 in
        let s = S.create l in
        copy_to_substring s 0 rope1;
        copy_to_substring s len1 l2;
        Concat(d2, len1 + len2, String s, l, r2)
      else
        let d = 1 + max (depth rope1) (depth rope2) in
        Concat(d, len1 + len2, rope1, len1, rope2)

let concat2 rope1 rope2 =
  let len1 = length rope1
  and len2 = length rope2 in
  let len = len1 + len2 in
  if len1 = 0 then rope2
  else if len2 = 0 then rope1
  else begin
    if len < len1 (* overflow *) then
      failwith "Rope.concat2: the length of the resulting rope exceeds max_int";
    let d = 1 + max (depth rope1) (depth rope2) in
    if d >= depth_to_rebalance then
      (* We will need to rebalance anyway, so do a fast concat *)
      balance (Concat(d, len, rope1, len1, rope2))
    else
      (* No automatic rebalancing *)
      concat2_nonempty rope1 rope2
  end
;;


(* Are lazy sub-rope nodes really needed? *)
(* This function assumes that [i], [len] define a valid sub-rope of
   the last arg. *)
let rec sub_rec i len = function
  | String s ->
      assert(i >= 0 && i <= String.length s - len);
      String(String.sub s i len)
  | Concat(_, rope_len, l, ll, r) ->
      let rl = rope_len - ll in
      let ri = i - ll in
      if ri >= 0 then
        if len = rl then r (* => ri = 0 -- full right sub-rope *)
        else sub_rec ri len r
      else
        let rlen = ri + len (* = i + len - ll *) in
        if rlen <= 0 then (* right sub-rope empty *)
          if len = ll then l (* => i = 0 -- full left sub-rope *)
          else sub_rec i len l
        else
          (* at least one char from the left and right sub-ropes *)
          let l' = if i = 0 then l else sub_rec i (-ri) l
          and r' = if rlen = rl then r else sub_rec 0 rlen r in
          let d = 1 + max (depth l') (depth r') in
          (* FIXME: do we have to use this opportunity to flatten
             some subtrees?  concat2 ?  In any case, the tree we get
             is no worse than the initial tree. *)
          Concat(d, len, l', -ri, r')

let sub r i len =
  if i < 0 || len < 0 || i > length r - len then invalid_arg "Rope.sub"
  else if len = 0 then empty
  else sub_rec i len r


(* Return the index of [c] in [s.[i .. i1-1]] plus the [offset] or
   [-1] if not found. *)
let rec index_string offset s i i1 c =
  if i >= i1 then -1
  else if s.[i] = c then offset + i
  else index_string offset s (i+1) i1 c;;

(* Return the index of [c] from [i] in the rope or a negative value
   if not found *)
let rec unsafe_index offset i c = function
  | String s -> index_string offset s i (String.length s) c
  | Concat(_, _, l,ll, r) ->
      if i >= ll then unsafe_index (offset + ll) (i - ll) c r
      else
        let li = unsafe_index offset i c l in
        if li >= 0 then li else unsafe_index (offset + ll) 0 c r

let index_from r i c =
  if i < 0 || i >= length r then invalid_arg "Rope.index_from" else
    let j = unsafe_index 0 i c r in
    if j >= 0 then j else raise Not_found

let index r c =
  let j = unsafe_index 0 0 c r in
  if j >= 0 then j else raise Not_found

let contains_from r i c =
  if i < 0 || i >= length r then invalid_arg "Rope.contains_from"
  else unsafe_index 0 i c r >= 0

let contains r c = unsafe_index 0 0 c r >= 0

(* Return the index of [c] in [s] (starting from the right) plus the
   [offset] or [-1] if not found. *)
let rec rindex_string offset s i c =
  if i < 0 then -1
  else if s.[i] = c then offset + i
  else rindex_string offset s (i - 1) c

let rec unsafe_rindex offset i c = function
  | String s -> rindex_string offset s i c
  | Concat(_, _, l,ll, r) ->
      if i < ll then unsafe_rindex offset i c l
      else
        let ri = unsafe_rindex (offset + ll) (i - ll) c r in
        if ri >= 0 then ri else unsafe_rindex offset (ll - 1) c l

let rindex_from r i c =
  if i < 0 || i > length r then invalid_arg "Rope.rindex_from"
  else
    let j = unsafe_rindex 0 i c r in
    if j >= 0 then j else raise Not_found

let rindex r c =
  let j = unsafe_rindex 0 (length r - 1) c r in
  if j >= 0 then j else raise Not_found

let rcontains_from r i c =
  if i < 0 || i >= length r then invalid_arg "Rope.rcontains_from"
  else unsafe_rindex 0 i c r >= 0


let rec map f = function
  | String s ->
      let s' = String.create(String.length s) in
      for i = 0 to String.length s - 1 do s'.[i] <- f s.[i] done;
      String s'
  | Concat(d, len, l, ll, r) -> Concat(d, len, map f l, ll, map f r)

let lowercase r = map Char.lowercase r
let uppercase r = map Char.uppercase r

let rec escaped = function
  | String s -> String(String.escaped s)
  | Concat(d, _, l, ll, r) ->
      let el = escaped l
      and er = escaped r in
      let ll = length el in
      Concat(d, ll + length er, el, ll, er)

let rec map1 f = function
  | Concat(d, len, l, ll, r) -> Concat(d, len, map1 f l, ll, r)
  | String s ->
      if s = "" then empty else begin
        let s' = String.copy s in
        s'.[0] <- f s.[0];
        String s'
      end

let capitalize r = map1 Char.uppercase r
let uncapitalize r = map1 Char.lowercase r

(* Iterator ---------------------------------------- *)
module Iterator = struct

  type t = {
    r: rope;
    len: int; (* = length r; avoids to recompute it again and again *)
    mutable i: int; (* current position in the rope; it is always a
                       valid position of the rope or [-1]. *)
    mutable current: string; (* local cache of current leaf *)
    mutable current_g0: int;
    (* global index of the beginning of current string.
       i0 = current_g0 + offset *)
    mutable current_g1: int;
    (* global index of the char past the current string.
       len = current_g1 - current_g0 *)
    mutable current_offset: int; (* = i0 - current_g0 *)
  }

  let rec set_current_for_index_rec itr g0 i = function
    | String s ->
        itr.current <- s;
        itr.current_g0 <- g0;
        itr.current_g1 <- g0 + String.length s;
        itr.current_offset <- - g0
    | Concat(_, _, l,ll, r) ->
        if i < ll then set_current_for_index_rec itr g0 i l
        else set_current_for_index_rec itr (g0 + ll) (i - ll) r

  let rope itr = itr.r

  let make r i0 =
    let len = length r in
    let itr =
      { r = r;
        len = len;
        i = i0;
        current = ""; current_offset = 0;
        current_g0 = 0; current_g1 = 0;
        (* empty range, important if not set! *)
      } in
    if i0 >= 0 && i0 < len then
      set_current_for_index_rec itr 0 i0 r; (* force [current] to be set *)
    itr

  let peek itr i =
    if i < 0 || i >= itr.len then raise(Out_of_bounds "Rope.Iterator.peek")
    else (
      if itr.current_g0 <= i && i < itr.current_g1 then
        String.get itr.current (i + itr.current_offset)
      else
        get itr.r i (* rope get *)
    )

  let get itr =
    let i = itr.i in
    if i < 0 || i >= itr.len then raise(Out_of_bounds "Rope.Iterator.get")
    else (
      if i < itr.current_g0 || i >= itr.current_g1 then
        set_current_for_index_rec itr 0 i itr.r; (* out of local bounds *)
      S.get itr.current (i + itr.current_offset)
    )

  let pos itr = itr.i

  let incr itr = itr.i <- itr.i + 1

  let decr itr = itr.i <- itr.i - 1

  let goto itr j = itr.i <- j

  let move itr k = itr.i <- itr.i + k
end

(* (In)equality ---------------------------------------- *)

exception Less
exception Greater

let compare r1 r2 =
  let len1 = length r1 and len2 = length r2 in
  let i1 = Iterator.make r1 0
  and i2 = Iterator.make r2 0 in
  try
    for i = 1 to min len1 len2 do (* on the common portion of [r1] and [r2] *)
      let c1 = Iterator.get i1 and c2 = Iterator.get i2 in
      if c1 < c2 then raise Less;
      if c1 > c2 then raise Greater;
      Iterator.incr i1;
      Iterator.incr i2;
    done;
    (* The strings are equal on their common portion, the shorter one
       is the smaller. *)
    compare (len1: int) len2
  with
  | Less -> -1
  | Greater -> 1
;;

(* Semantically equivalent to [compare r1 r2 = 0] but specialized
   implementation for speed. *)
let equal r1 r2 =
  let len1 = length r1 and len2 = length r2 in
  if len1 <> len2 then false else (
    let i1 = Iterator.make r1 0
    and i2 = Iterator.make r2 0 in
    try
      for i = 1 to len1 do (* len1 times *)
        if Iterator.get i1 <> Iterator.get i2 then raise Exit;
        Iterator.incr i1;
        Iterator.incr i2;
      done;
      true
    with Exit -> false
  )

(* KMP search algo ---------------------------------------- *)
let init_next p =
  let m = String.length p in
  let next = Array.create m 0 in
  let i = ref 1 and j = ref 0 in
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.(!i) <- !j end
    else if !j = 0 then begin incr i; next.(!i) <- 0 end else j := next.(!j)
  done;
  next

let search_forward_string p =
  if String.length p > Sys.max_array_length then
    failwith "Rope.search_forward: string to search too long";
  let next = init_next p
  and m = String.length p in
  fun rope i0 ->
    let i = Iterator.make rope i0
    and j = ref 0 in
    (try
       (* The iterator will raise an exception of we go beyond the
          length of the rope. *)
       while !j < m do
         if p.[!j] = Iterator.get i then begin Iterator.incr i; incr j end
         else if !j = 0 then Iterator.incr i else j := next.(!j)
       done;
     with Out_of_bounds _ -> ());
    if !j >= m then Iterator.pos i - m else raise Not_found


(* Buffer ---------------------------------------- *)

module Buffer = struct

  (* The content of the buffer consists of the forest concatenated in
     decreasing order plus (at the end) the part stored in [buf]:
     [forest.(max_depth-1) ^ ... ^ forest.(1) ^ forest.(0) ^ S.sub buf 0 pos]
  *)
  type t = {
    mutable buf: string;
    buf_len: int; (* = S.length buf; must be > 0 *)
    mutable pos: int;
    mutable length: int; (* the length of the rope contained in this buffer
                            -- including the part in the forest *)
    forest: rope array; (* keeping the partial rope in a forest will
                           ensure it is balanced at the end. *)
  }

  (* We will not allocate big buffers, if we exceed the buffer length,
     we will cut into small chunks and add it directly to the forest.  *)
  let create n =
    let n = if n < 1 then small_rope_length else n in
    { buf = S.create n;
      buf_len = n;
      pos = 0;
      length = 0;
      forest = Array.make max_depth empty;
    }

  let clear b =
    b.pos <- 0;
    b.length <- 0;
    Array.fill b.forest 0 max_depth empty

  (* [reset] is no different from [clear] because we do not grow the buffer. *)
  let reset b = clear b

  let add_char b c =
    if b.length = max_int then failwith "Rope.Buffer.add_char: \
	buffer length will exceed the int range";
    if b.pos >= b.buf_len then (
      (* Buffer full, add it to the forest and allocate a new one: *)
      add_nonempty_to_forest b.forest (String b.buf);
      b.buf <- S.create b.buf_len;
      b.buf.[0] <- c;
      b.pos <- 1;
    )
    else (
      b.buf.[b.pos] <- c;
      b.pos <- b.pos + 1;
    );
    b.length <- b.length + 1

  let unsafe_add_substring b s ofs len =
    (* Beware of int overflow *)
    if b.length > max_int - len then failwith "Rope.Buffer.add_substring: \
	buffer length will exceed the int range";
    let buf_left = b.buf_len - b.pos in
    if len <= buf_left then (
      (* Enough space in [buf] to hold the substring of [s]. *)
      S.blit s ofs b.buf b.pos len;
      b.pos <- b.pos + len;
    )
    else (
      (* Complete [buf] and add it to the forest: *)
      S.blit s ofs b.buf b.pos buf_left;
      add_nonempty_to_forest b.forest (String b.buf);
      b.buf <- S.create b.buf_len;
      b.pos <- 0;
      (* Add the remaining of [s] to to forest (it is already
         balanced by of_substring, so we add is as such): *)
      let s = unsafe_of_substring s (ofs + buf_left) (len - buf_left) in
      add_nonempty_to_forest b.forest s
    );
    b.length <- b.length + len

  let add_substring b s ofs len =
    if ofs < 0 || len < 0 || ofs > S.length s - len then
      invalid_arg "Rope.Buffer.add_substring";
    unsafe_add_substring b s ofs len

  let add_string b s = unsafe_add_substring b s 0 (S.length s)

  let add_rope b (r: rope) =
    if is_not_empty r then (
      let len = length r in
      if b.length > max_int - len then failwith "Rope.Buffer.add_rope: \
	buffer length will exceed the int range";
      (* First add the part hold by [buf]: *)
      add_to_forest b.forest (String(S.sub b.buf 0 b.pos));
      b.pos <- 0;
      (* I thought [balance_insert b.forest r] was going to rebalance
         [r] taking into account the content already in the buffer but
         it does not seem faster.  We take the decision to possibly
         rebalance when the content is asked. *)
      add_nonempty_to_forest b.forest r; (* [r] not empty *)
      b.length <- b.length + len
    )
  ;;

  let add_buffer b b2 =
    if b.length > max_int - b2.length then failwith "Rope.Buffer.add_buffer: \
	buffer length will exceed the int range";
    add_to_forest b.forest (String(S.sub b.buf 0 b.pos));
    b.pos <- 0;
    let forest = b.forest in
    let forest2 = b2.forest in
    for i = Array.length b2.forest - 1 to 0 do
      add_to_forest forest forest2.(i)
    done;
    b.length <- b.length + b2.length
  ;;

  let add_channel b ic len =
    if b.length > max_int - len then failwith "Rope.Buffer.add_channel: \
	buffer length will exceed the int range";
    let buf_left = b.buf_len - b.pos in
    if len <= buf_left then (
      (* Enough space in [buf] to hold the input from the channel. *)
      really_input ic b.buf b.pos len;
      b.pos <- b.pos + len;
    )
    else (
      (* [len > buf_left]. Complete [buf] and add it to the forest: *)
      really_input ic b.buf b.pos buf_left;
      add_nonempty_to_forest b.forest (String b.buf);
      (* Read the remaining from the channel *)
      let len = ref(len - buf_left) in
      while !len >= b.buf_len do
        let s = S.create b.buf_len in
        really_input ic s 0 b.buf_len;
        add_nonempty_to_forest b.forest (String s);
        len := !len - b.buf_len;
      done;
      (* [!len < b.buf_len] to read, put them into a new [buf]: *)
      let s = S.create b.buf_len in
      really_input ic s 0 !len;
      b.buf <- s;
      b.pos <- !len;
    );
    b.length <- b.length + len
  ;;

  (* Search for the nth element in [forest.(i ..)] of total length [len] *)
  let rec nth_forest forest k i len =
    assert(k <= Array.length forest);
    let r = forest.(k) in (* possibly empty *)
    let ofs = len - length r in (* offset of [r] in the full rope *)
    if i >= ofs then get r (i - ofs)
    else nth_forest forest (k + 1) i ofs

  let nth b i =
    if i < 0 || i >= b.length then raise(Out_of_bounds "Rope.Buffer.nth");
    let forest_len = b.length - b.pos in
    if i >= forest_len then b.buf.[i - forest_len]
    else nth_forest b.forest 0 i forest_len
  ;;

  (* Return a rope, [buf] must be duplicated as it becomes part of the
     rope, thus we duplicate it as ropes are immutable.  What we do is
     very close to [add_nonempty_to_forest] followed by
     [concat_forest] except that we do not modify the forest and we
     select a sub-rope.  Assume [len > 0] -- and [i0 >= 0]. *)
  let unsafe_sub (b: t) i0 len =
    let i1 = i0 + len in (* 1 char past subrope *)
    let forest_len = b.length - b.pos in
    let buf_i1 = i1 - forest_len in
    if buf_i1 >= len then
      (* The subrope is entirely in [buf] *)
      String(S.sub b.buf (i0 - forest_len) len)
    else begin
      let n = ref 0 in
      let sum = ref empty in
      if buf_i1 > 0 then (
        (* At least one char in [buf] and at least one in the forest.
           Concat the ropes of inferior length and append the part of [buf] *)
        let rem_len = len - buf_i1 in
        while buf_i1 > min_length.(!n + 1) && length !sum < rem_len do
          sum := balance_concat b.forest.(!n) !sum;
          incr n
        done;
        sum := balance_concat !sum (String(S.sub b.buf 0 buf_i1))
      )
      else (
        (* Subrope in the forest.  Skip the forest elements until
           the last chunk of the sub-rope is found.  Since [0 < len
           <= forest_len], there exists a nonempty rope in the forest. *)
        let j = ref buf_i1 in (* <= 0 *)
        while !j <= 0 do j := !j + length b.forest.(!n); incr n done;
        sum := sub b.forest.(!n - 1) 0 !j (* init. with proper subrope *)
      );
      (* Add more forest elements until we get at least the desired length *)
      while length !sum < len do
        assert(!n < max_depth);
        sum := balance_concat b.forest.(!n) !sum;
        incr n
      done;
      let extra = length !sum - len in
      if extra = 0 then !sum else sub !sum extra len
    end

  let sub b i len =
    if i < 0 || len < 0 || i > b.length - len then
      invalid_arg "Rope.Buffer.sub";
    if len = 0 then empty
    else balance_if_needed(unsafe_sub b i len)

  let contents b =
    if b.length = 0 then empty
    else balance_if_needed(unsafe_sub b 0 b.length)


  let length b = b.length
end


(* Using the Buffer module should be more efficient than sucessive
   concatenations and ensures that the final rope is balanced. *)
let concat sep = function
  | [] -> empty
  | r0 :: tl ->
      let b = Buffer.create 1 in (* [buf] will not be used as we add ropes *)
      Buffer.add_rope b r0;
      List.iter (fun r -> Buffer.add_rope b sep; Buffer.add_rope b r) tl;
      Buffer.contents b


(* Input/output -- modeled on Pervasive *)

(* Imported from pervasives.ml: *)
external input_scan_line : in_channel -> int = "caml_ml_input_scan_line"

let input_line ?(leaf_size=128) chan =
  let b = Buffer.create leaf_size in
  let rec scan () =
    let n = input_scan_line chan in
    if n = 0 then                   (* n = 0: we are at EOF *)
      if Buffer.length b = 0 then raise End_of_file
      else Buffer.contents b
    else if n > 0 then (            (* n > 0: newline found in buffer *)
      Buffer.add_channel b chan (n-1);
      ignore (input_char chan);           (* skip the newline *)
      Buffer.contents b
    )
    else (                          (* n < 0: newline not found *)
      Buffer.add_channel b chan (-n);
      scan ()
    )
  in scan()
;;

let read_line () = flush stdout; input_line stdin

let rec output_string fh = function
  | String s -> output fh s 0 (String.length s)
  | Concat(_, _, l,_, r) -> output_string fh l; output_string fh r
;;

let output_rope = output_string

let print_string rope = output_string stdout rope
let print_endline rope = output_string stdout rope; print_newline()

let prerr_string rope = output_string stderr rope
let prerr_endline rope = output_string stderr rope; prerr_newline()


(**/**)
let rec number_leaves = function
  | String _ -> 1
  | Concat(_,_, l,_, r) -> number_leaves l + number_leaves r

let rec number_concat = function
  | String _ -> 0
  | Concat(_,_, l,_, r) -> 1 + number_concat l + number_concat r

(* For debugging and judging balancing *)
let print =
  let rec print prefix = function
    | String s ->
        Pervasives.print_string prefix;
        Pervasives.print_string s;
        Pervasives.print_string "\n"
    | Concat(_,_, l,_, r) ->
        let prefixl = prefix ^ if depth l = 0 then "/" else " " in
        let prefixr = prefix ^ if depth r = 0 then "\\" else " " in
        print prefixl l;
        print prefixr r;
  in print ""

(**/**)

(* Toplevel ---------------------------------------- *)

module Rope_toploop = struct
  open Format

  let max_display_length = ref 400
    (* When displaying, truncate strings that are longer than this. *)

  let ellipsis = ref "..."
    (* ellipsis for ropes longer than max_display_length.  User changeable.  *)

  (* Return [max_len - length r].  *)
  let rec printer_lim max_len (fm:formatter) r =
    if max_len > 0 then
      match r with
      | Concat(_,_, l,_, r) ->
          let to_be_printed = printer_lim max_len fm l in
          printer_lim to_be_printed fm r
      | String s ->
          let len = String.length s in
          let l = if len <= max_len then len else max_len in
          pp_print_string fm (String.sub s 0 l);
          max_len - len
    else max_len

  let printer fm r =
    pp_print_string fm "\"";
    let to_be_printed = printer_lim !max_display_length fm r in
    pp_print_string fm "\"";
    if to_be_printed < 0 then pp_print_string fm !ellipsis
end

module Regexp = struct


end
