(*
 * Copyright (c) 2013-2015 David Sheets <sheets@alum.mit.edu>
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

(* TODO: shouldn't expose? *)
type 'a deque = 'a list * 'a list

module type TEMPLATE = sig
  type hole
  type prov

  val signals_of_hole : prov:prov -> hole -> Xmlm.signal list
end

module type S = sig
  type hole
  type prov
  type t
  type 'a patch = 'a -> prov -> hole -> ('a * t) option
  type 'a sink = prov -> 'a -> Xmlm.signal list -> 'a

  val of_list : prov:prov -> Xmlm.signal list -> t

  val make_hole : prov:prov -> hole -> t

  val make_attrs : prov:prov -> Xmlm.tag -> (Xmlm.name * t) list -> t
  val in_attrs : t -> bool
  val with_attr : t -> Xmlm.name * t -> t

  val concat : t -> t -> t
  val ( ++ ) : t -> t -> t
  val ( +? ) : t option -> t -> t

  val empty : t

  val optimize : t -> t

  val of_tree :
    prov:prov ->
    ([< `Data of string | `El of Xmlm.tag * 'b list ] as 'b) list -> t

  val holes : t -> hole list

  val map_prov : (prov -> prov) -> t -> t

  val patch : 'a patch -> 'a -> t -> t

  val to_stream :
    patch:'a patch -> sink:(prov -> 'out -> Xmlm.signal list -> 'out) -> 'a ->
    'out -> t -> 'out

  val to_list : patch:'a patch -> 'a -> t -> Xmlm.signal list
end

module Make(M : TEMPLATE) : S with type hole = M.hole and type prov = M.prov
