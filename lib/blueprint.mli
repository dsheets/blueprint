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

type error = [
  | `Empty_hole of string
  | `Unknown_tag of string
  | `Missing_attribute of string * string
  | `Data_after_root
  | `Element_after_root
]

exception Error of error

type prov = Stream of string | File of string

val xmlns : string

val error_message : error -> string

module Hole : sig
  type 'value t = Named of string | Valued of string * 'value
end

module HoleMap : Map.S

module HoleTable : Hashtbl.S

type t
type template = (hole, prov) XmlRope.t
and hole = valued Hole.t
and valued = Default of template | Typed of string * template
(* TODO: shouldn't expose? *)
and bindings =
| Generator of (hole -> template option) * bindings option
| Map of template HoleMap.t * bindings option
| Table of template HoleTable.t * bindings option

val template : t -> template

val bindings : t -> bindings

val append : bindings -> bindings -> bindings

val of_stream :
  prov:prov -> source:('acc -> 'acc * Xmlm.signal option) -> 'acc -> 'acc * t

val xml_source : Xmlm.input -> Xmlm.input * Xmlm.signal option

val bind_hole : bindings -> hole -> template

val bind :
  sink:(prov -> 'acc -> Xmlm.signal list -> 'acc) ->
  'acc -> bindings -> template -> 'acc
