(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type 'a deque = 'a list * 'a list

(* TODO: could include indexes for fast traversal/lookup *)
type ('hole, 'prov) t =
| Literal of 'prov * Xmlm.signal deque
| Wrap of 'prov * Xmlm.signal deque * ('hole, 'prov) t * Xmlm.signal deque
| Hole of 'prov * 'hole
| Sequence of ('hole, 'prov) t deque

let of_list ~prov list = Literal (prov, (list,[]))

(* TODO: could optimize if prov = prov' *)
let wrap ~prov start v fin = Wrap (prov, start, v, fin)

let hole ~prov hole = Hole (prov, hole)

(* TODO: could optimize if prov = prov' *)
let concat a b = match a, b with
  | atom, Sequence (fropes, rropes) -> Sequence (atom::fropes, rropes)
  | Sequence (fropes, rropes), atom -> Sequence (fropes, atom::rropes)
  | atom, atom' -> Sequence ([atom; atom'], [])

let (++) = concat

let empty = Sequence ([],[])

(* TODO: Should re-arrange for optimal access *)
let optimize x = x

let of_tree ~prov xml =
  let rec aux acc = function
    | `Data d -> (`Data d)::acc
    | `El (tag,contents) ->
      `El_end::(List.fold_left aux ((`El_start tag)::acc) contents)
  in
  Literal (prov, List.(rev (fold_left aux [] xml), []))

let holes rope =
  let rec aux acc = function
    | Literal (_,_) -> acc
    | Hole (_, hole) -> hole::acc
    | Wrap (_, _, r, _) -> aux acc r
    | Sequence (fropes, rropes) ->
      List.(fold_left aux (fold_left aux acc fropes) (rev rropes))
  in
  List.rev (aux [] rope)

(* TODO: should also optimize? *)
let patch f =
  let rec aux = function
    | Literal (_,_) as r -> r
    | Wrap (prov,l,v,r) -> Wrap (prov,l,aux v,r)
    | Sequence (fropes,rropes) ->
      Sequence (List.map aux fropes, List.map aux rropes)
    | Hole (prov, hole) as h -> match f prov hole with
      | None -> h
      | Some t -> aux t
  in
  fun rope -> aux rope

let rec to_stream ~patch ~sink =
  let rec aux acc = function
    | Literal (prov, (fs,rs)) -> sink prov (sink prov acc fs) (List.rev rs)
    | Hole (prov, h) ->
      let acc, rope = patch prov acc h in
      aux acc rope
    | Wrap (prov, (fs,rs), v, (fs',rs')) ->
      let acc = sink prov (sink prov acc fs) (List.rev rs) in
      let acc = aux acc v in
      sink prov (sink prov acc fs') (List.rev rs')
    | Sequence (fropes, rropes) ->
      List.(fold_left aux (fold_left aux acc fropes) (rev rropes))
  in
  fun acc rope -> aux acc rope

let to_list ~patch rope =
  let sink _ acc s = List.rev_append s acc in
  List.rev (to_stream ~patch ~sink [] rope)
