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

  val patch : (prov -> hole -> t option) -> t -> t

  val to_stream :
    patch:(prov -> 'acc -> hole -> 'acc * t) ->
    sink:(prov -> 'acc -> Xmlm.signal list -> 'acc) ->
    'acc -> t -> 'acc

  val to_list :
    patch:(prov -> Xmlm.signal list -> hole -> Xmlm.signal list * t) ->
    t -> Xmlm.signal list
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
  let patch f rope =
    let rec aux tbl = function
      | Literal (_,_) as r -> r
      | Wrap (prov,l,v,r) -> Wrap (prov,l,aux tbl v,r)
      | Sequence (fropes,rropes) ->
        Sequence (List.map (aux tbl) fropes, List.map (aux tbl) rropes)
      | Hole (prov, hole) as h -> match f prov hole with
        | None -> h
        | Some t ->
          if Hashtbl.mem tbl t
          then h
          else
            let tbl = Hashtbl.copy tbl in
            Hashtbl.replace tbl t ();
            aux tbl t
    in
    let tbl = Hashtbl.create 8 in
    Hashtbl.replace tbl rope ();
    aux tbl rope

  (* TODO: should use patch? *)
  let rec to_stream ~patch ~sink =
    let rec aux tbl acc = function
      | Literal (prov, (fs,rs)) -> sink prov (sink prov acc fs) (List.rev rs)
      | Hole (prov, h) ->
        let acc', rope = patch prov acc h in
        if Hashtbl.mem tbl rope
        then sink prov acc (M.signals_of_hole prov h)
        else
          let tbl = Hashtbl.copy tbl in
          Hashtbl.replace tbl rope ();
          aux tbl acc' rope
      | Wrap (prov, (fs,rs), v, (fs',rs')) ->
        let acc = sink prov (sink prov acc fs) (List.rev rs) in
        let acc = aux tbl acc v in
        sink prov (sink prov acc fs') (List.rev rs')
      | Sequence (fropes, rropes) ->
        List.(fold_left (aux tbl) (fold_left (aux tbl) acc fropes) (rev rropes))
    in
    fun acc rope ->
      let tbl = Hashtbl.create 8 in
      Hashtbl.replace tbl rope ();
      aux tbl acc rope

  let to_list ~patch rope =
    let sink _ acc s = List.rev_append s acc in
    List.rev (to_stream ~patch ~sink [] rope)
end
