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

type ('hole, 'prov) t

val of_list : prov:'p -> Xmlm.signal list -> ('h, 'p) t

val wrap :
  prov:'p ->
  Xmlm.signal deque -> ('h, 'p) t -> Xmlm.signal deque -> ('h, 'p) t

val hole : prov:'p -> 'h -> ('h, 'p) t

val concat : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
val ( ++ ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val empty : ('a, 'b) t

val optimize : ('a, 'b) t -> ('a, 'b) t

val of_tree :
  prov:'p ->
  ([< `Data of string | `El of Xmlm.tag * 'b list ] as 'b) list -> ('h, 'p) t

val holes : ('h, 'p) t -> 'h list

val patch : ('p -> 'h -> ('h, 'p) t option) -> ('h, 'p) t -> ('h, 'p) t

val to_stream :
  patch:('prov -> 'acc -> 'hole -> 'acc * ('hole, 'prov) t) ->
  sink:('prov -> 'acc -> Xmlm.signal list -> 'acc) ->
  'acc -> ('hole, 'prov) t -> 'acc

val to_list :
  patch:('prov -> Xmlm.signal list -> 'hole ->
         Xmlm.signal list * ('hole, 'prov) t) ->
  ('hole, 'prov) t -> Xmlm.signal list
