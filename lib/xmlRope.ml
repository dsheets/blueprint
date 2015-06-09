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

module type TEMPLATE = sig
  type hole
  type prov

  val signals_of_hole : prov -> hole -> Xmlm.signal list
end

module type S = sig
  type hole
  type prov
  type t
  type 'a patch = 'a -> prov -> hole -> 'a * t

  val of_list : prov:prov -> Xmlm.signal list -> t

  val wrap : prov:prov -> Xmlm.signal deque -> t -> Xmlm.signal deque -> t

  val hole : prov:prov -> hole -> t

  val concat : t -> t -> t
  val ( ++ ) : t -> t -> t

  val empty : t

  val optimize : t -> t

  val of_tree :
    prov:prov ->
    ([< `Data of string | `El of Xmlm.tag * 'b list ] as 'b) list -> t

  val holes : t -> hole list

  val patch : 'a patch -> 'a -> t -> t

  val to_stream :
    patch:'a patch -> sink:(prov -> 'out -> Xmlm.signal list -> 'out) -> 'a ->
    'out -> t -> 'out

  val to_list : patch:'a patch -> 'a -> t -> Xmlm.signal list
end

module Make(M : TEMPLATE) : S with type hole = M.hole and type prov = M.prov =
struct
  type hole = M.hole
  type prov = M.prov

  (* TODO: could include indexes for fast traversal/lookup *)
  type t =
    | Literal of M.prov * Xmlm.signal deque
    | Wrap of M.prov * Xmlm.signal deque * t * Xmlm.signal deque
    | Hole of M.prov * M.hole
    | Sequence of t deque

  type 'a patch = 'a -> prov -> hole -> 'a * t

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

  (* TODO: should use something other than Hashtbl? *)
  (* TODO: should also optimize? *)
  let patch f acc rope =
    let rec aux ((acc,tbl) as c) = function
      | Literal (_,_) as r -> r
      | Wrap (prov,l,v,r) -> Wrap (prov,l,aux c v,r)
      | Sequence (fropes,rropes) ->
        Sequence (List.map (aux c) fropes, List.map (aux c) rropes)
      | Hole (prov, hole) as h ->
        let acc, t = f acc prov hole in
        if Hashtbl.mem tbl t
        then h
        else
          let tbl = Hashtbl.copy tbl in
          Hashtbl.replace tbl t ();
          aux (acc,tbl) t
    in
    let tbl = Hashtbl.create 8 in
    Hashtbl.replace tbl rope ();
    aux (acc,tbl) rope

  (* TODO: should use patch? *)
  let rec to_stream ~patch ~sink =
    let rec aux ((acc,tbl) as c) out = function
      | Literal (prov, (fs,rs)) -> sink prov (sink prov out fs) (List.rev rs)
      | Hole (prov, h) ->
        let acc, rope = patch acc prov h in
        if Hashtbl.mem tbl rope
        then sink prov out (M.signals_of_hole prov h)
        else
          let tbl = Hashtbl.copy tbl in
          Hashtbl.replace tbl rope ();
          aux (acc,tbl) out rope
      | Wrap (prov, (fs,rs), v, (fs',rs')) ->
        let out = sink prov (sink prov out fs) (List.rev rs) in
        let out = aux c out v in
        sink prov (sink prov out fs') (List.rev rs')
      | Sequence (fropes, rropes) ->
        List.(fold_left (aux c) (fold_left (aux c) out fropes) (rev rropes))
    in
    fun acc out rope ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.replace tbl rope ();
      aux (acc, tbl) out rope

  let to_list ~patch acc rope =
    let sink _ acc s = List.rev_append s acc in
    List.rev (to_stream ~patch ~sink acc [] rope)
end
