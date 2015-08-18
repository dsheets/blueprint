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

module Prov : sig
  type loc = int * int
  type src = Stream of string | File of string
  type t = {
    src : src;
    loc : loc option;
    incl : (string * t) option;
  }
end

type binding_error = [
  | `Empty_hole of Prov.t option * string
  | `Dangling_link of Prov.t * string * string
]

type expansion_error = [
  | binding_error
  | `Disallowed_expansion of Prov.t * string
]

type error = [
  | expansion_error
  | `Disallowed_traversal of Prov.t * string
  | `Bad_ident of Prov.loc * string
  | `Unknown_tag of Prov.loc * string
  | `Missing_attribute of Prov.loc option * string * string
  | `Floating_attr of Prov.loc * string
  | `Floating_else of Prov.loc
  | `Data_after_root of Prov.loc option
  | `Element_after_root of Prov.loc option
]

exception Error of error

type signal = Prov.loc * Xmlm.signal

val xmlns : string

val xmlns_map_default : string -> string option

val error_message : error -> string

module Ident : sig
  type absolute = [ `Absolute ]
  type relative = [ `Relative ]
  type any = [ absolute | relative ]
  type 'a t
  type relative_t = relative t
  type absolute_t = absolute t
  type any_t = any t
end

module Scope : sig
  type link = {
    base : Ident.absolute_t;
    target : Ident.any_t;
    location : Prov.t;
  }
  type 'rope t
  type 'rope obj =
    | Scope of 'rope t
    | Link of link
    | Stack of 'rope obj list

  val empty : 'a t

  val template : 'rope t -> 'rope option Lazy.t

  val overlay : 'rope t -> 'rope t -> 'rope t

  val find : 'rope t -> string -> 'rope obj option
end

module Hole : sig
  type ('rope, 'value) t
end

module Env : sig
  type 'rope t

  val create : 'rope Scope.t -> Ident.relative_t -> 'rope t
end

module rec Template :
  (XmlRope.TEMPLATE with type hole = (Rope.t, Rope.t) Hole.t
                     and type prov = Prov.t
                     and type env = Rope.t Env.t)
and Rope : XmlRope.S
  with type hole = Template.hole
   and type prov = Template.prov
   and type env  = Template.env

type t = Rope.t Scope.t

val default_rope : Rope.t option Lazy.t -> Rope.t

val of_stream :
  prov:Prov.t -> source:('acc -> 'acc * signal option) -> 'acc -> 'acc * t

val xml_source : Xmlm.input -> Xmlm.input * signal option

val bind :
  ?partial:bool ->
  sink:'acc Rope.sink ->
  'acc -> Rope.t Scope.t -> Rope.t -> 'acc

val xml_sink :
  ?partial:bool -> unit ->
  (Prov.t -> Xmlm.output -> Xmlm.signal list -> Xmlm.output)

val buffer_sink :
  ?partial:bool -> Buffer.t ->
  (Prov.t -> unit -> Xmlm.signal list -> unit)

val bind_to_output :
  ?partial:bool -> out_channel -> Rope.t Scope.t -> Rope.t -> unit
