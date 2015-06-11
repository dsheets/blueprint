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

val xmlns_map_default : string -> string option

val error_message : error -> string

module HoleMap : Map.S

module HoleTable : Hashtbl.S

module Bindings : sig
  (* TODO: shouldn't expose? *)
  type 'rope t =
    | Generator of (string list -> 'rope option) * 'rope t option
    | Map of 'rope HoleMap.t * 'rope t option
    | Table of 'rope HoleTable.t * 'rope t option

  val empty : 'a t
  val append : 'a t -> 'a t -> 'a t
end

module Hole : sig
  (* TODO: expose? *)
  type ('rope, 'value) t = {
    name : string;
    default : 'value option;
    env : 'rope Bindings.t;
  }
end

type 'a valued = Default of 'a | Typed of string * 'a

module rec Template :
  (XmlRope.TEMPLATE with type hole = (Rope.t, Rope.t valued) Hole.t
                     and type prov = prov)
and Rope : XmlRope.S with
  type hole = Template.hole and type prov = Template.prov

type t

val template : t -> Rope.t

val bindings : t -> Rope.t Bindings.t

val of_stream :
  prov:prov -> source:('acc -> 'acc * Xmlm.signal option) -> 'acc -> 'acc * t

val xml_source : Xmlm.input -> Xmlm.input * Xmlm.signal option

val bind_hole : Rope.t Bindings.t -> Template.hole -> Rope.t option

val bind :
  ?incomplete:bool ->
  sink:(prov -> 'acc -> Xmlm.signal list -> 'acc) ->
  'acc -> Rope.t Bindings.t -> Rope.t -> 'acc
