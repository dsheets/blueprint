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

(* This is an extremely simple templating system based on XML ropes
   with named holes. *)

(* TODO: these should carry the location of the error and source *)
type error = [
  | `Empty_hole of string
  | `Unknown_tag of string
  | `Missing_attribute of string * string
  | `Data_after_root
  | `Element_after_root
]

exception Error of error

let xmlns = "https://opam.ocaml.org/packages/blueprint/xmlns/0/#"

let error_message : error -> string = function
  | `Empty_hole name -> "No value for hole named '"^name^"'"
  | `Unknown_tag tag -> "Unknown tag '"^tag^"'"
  | `Missing_attribute (tag,attr) ->
    "Tag '"^tag^"' is missing attribute '"^attr^"'"
  | `Data_after_root -> "data are not allowed after the root element"
  | `Element_after_root -> "tags are not allowed after the root element"

let rec compare_string_list sl sl' = match sl, sl' with
  | [], [] -> 0
  | _::_, [] -> 1
  | [], _::_ -> -1
  | h::t, h'::t' -> match String.compare h h' with
    | 0 -> compare t t'
    | x -> x

(* TODO: Maybe these should just be string/name based. *)
module HoleMap = Map.Make(struct
  type t = string list
  let compare = compare_string_list
end)

module HoleTable = Hashtbl.Make(struct
  type t = string list
  let equal a b = compare_string_list a b = 0
  let hash = Hashtbl.hash
end)

module Bindings = struct
(* TODO: This is a bit weird... *)
  type 'rope t =
    | Generator of (string list -> 'rope option) * 'rope t option
    | Map of 'rope HoleMap.t * 'rope t option
    | Table of 'rope HoleTable.t * 'rope t option

  let empty = Map (HoleMap.empty, None)

  (* TODO: this is horrible *)
  let rec append a b = match a with
    | Generator (f,None) -> Generator (f,Some b)
    | Generator (f,Some bs) -> Generator (f,Some (append bs b))
    | Map (m,None) -> Map (m,Some b)
    | Map (m,Some bs) -> Map (m,Some (append bs b))
    | Table (t,None) -> Table (t,Some b)
    | Table (t,Some bs) -> Table (t, Some (append bs b))

  (* TODO: also horrible *)
  let rec get bindings name = match bindings with
    | Generator (g, more) -> begin
        match g name with
        | Some t -> Some t
        | None -> match more with
          | Some bindings -> get bindings name
          | None -> None
      end
    | Map (map, more) -> begin
        try Some (HoleMap.find name map)
        with Not_found -> match more with
          | Some bindings -> get bindings name
          | None -> None
      end
    | Table (tbl, more) -> begin
        try Some (HoleTable.find tbl name)
        with Not_found -> match more with
          | Some bindings -> get bindings name
          | None -> None
      end
end

module Hole = struct
  type ('rope, 'value) t = {
    name : string;
    default : 'value option;
    env : 'rope Bindings.t;
  }

  let name { name } = name

  let named name = { name; default = None; env = Bindings.empty }
  let valued name value = { name; default = Some value; env = Bindings.empty }
end

(* TODO: What about lexical info? values are assumed to be static per
   source *)
type prov =
  | Stream of string
  | File of string
type prov' = prov

type 'a valued =
  | Default of 'a
  | Typed of string * 'a

module rec Template :
  XmlRope.TEMPLATE with type hole = (Rope.t, Rope.t valued) Hole.t
                    and type prov = prov
= struct
  type hole = (Rope.t, Rope.t valued) Hole.t
  type prov = prov'

  (* TODO: defaults, bindings? *)
  let signals_of_hole _prov = Hole.(function
    | { name } -> [
        `El_start ((xmlns,"insert"),[("","name"),name]); `El_end;
      ]
  )
end
and Rope : XmlRope.S
  with type hole = Template.hole
   and type prov = Template.prov
  = XmlRope.Make(Template)

type t = {
  rope : Rope.t;
  bindings : Rope.t Bindings.t;
}

let template { rope } = rope
let bindings { bindings } = bindings

(* TODO: match? *)
let declare name rope bindings =
  Bindings.Map (HoleMap.singleton name rope, Some bindings)

let get_attr tag attrs attr =
  try List.assoc ("",attr) attrs
  with Not_found -> raise (Error (`Missing_attribute (tag, attr)))

module XmlStack = struct
  type frame = int * (int * Rope.t Bindings.t) list
  type t = frame list

  let consumep = function (0,_)::_ -> true | [] | _::_ -> false
  let push stack = (0,[])::stack
  let incr = function [] -> [] | (h,s)::t -> (h+1,s)::t
  let decr bindings = function
    | [] -> [], bindings
    | (h,(d,b)::bs)::t when h = d -> (h-1,bs)::t, b
    | (h,bs)::t -> (h-1,bs)::t, bindings
  let pop = function [] -> [] | _::t -> t

  let save_bindings bindings = function
    | (d,[])::s -> (d,[d,bindings])::s
    | ((d,(ld,_)::_)::_) as s when ld = d -> s
    | (d,bs)::s -> (d,(d,bindings)::bs)::s
    | [] -> []
end

let rtrim = function
  | (`Data s)::r ->
    let len = String.length s in
    let rec last i =
      let i = i - 1 in
      if i < 0 then 0
      else match String.get s i with
        | ' '  -> last i
        | '\n' -> nl 1 i
        | _    -> i + 1
    and nl k i =
      if k > 0
      then
        let i = i - 1 in
        if i < 0 then 0
        else match String.get s i with
          | '\n' -> nl (k - 1) i
          | _    -> i + 1
      else i
    in
    if len > 0
    then (`Data (String.sub s 0 (last len)))::r
    else (`Data s)::r
  | s -> s

let of_stream ~prov ~source =
  let rec run stack seq ({ rope; bindings } as b) = function
    | acc, None ->
      acc, { b with rope=Rope.(rope ++ (of_list ~prov (List.rev seq))) }
    | acc, Some el -> match el with
      | `El_start ((ns,el),attrs) when ns=xmlns ->
        handle stack seq b acc attrs el
      | `El_start el ->
        run (XmlStack.incr stack) ((`El_start el)::seq) b (source acc)
      | `El_end when XmlStack.consumep stack ->
        begin match XmlStack.pop stack with
          | [] ->
            acc, { b with rope=Rope.(rope ++ (of_list ~prov (List.rev seq))) }
          | stack -> run stack seq b (source acc)
        end
      | `El_end ->
        let stack, bindings = XmlStack.decr bindings stack in
        run stack (`El_end::seq) { b with bindings } (source acc)
      | (`Dtd _ | `Data _) as signal ->
        run stack (signal::seq) b (source acc)
  and handle stack seq ({ rope; bindings } as b) acc attrs = Rope.(function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      let name = get_attr "insert" attrs "name" in
      begin match Bindings.get bindings [name] with
        | Some template ->
          let template = patch (fun _prov hole ->
            Bindings.get bindings [Hole.name hole]
          ) template in
          let default = { b with rope = empty } in
          let acc, _default = run [0,[]] [] default (source acc) in
          let rope = rope ++ literal ++ template in
          run stack [] { b with rope } (source acc)
        | None ->
          let hole = Hole.named name in
          let b = { b with rope = rope ++ literal ++ (Rope.hole ~prov hole) } in
          match source acc with
          | acc, None -> acc, b
          | acc, Some `El_end ->
            run (XmlStack.push stack) [] b (acc, Some `El_end)
          | acc, Some signal ->
            let default = { b with rope = empty } in
            let acc, default = run [0,[]] [] default (acc, Some signal) in
            let hole = Hole.valued name (Default default.rope) in
            let rope = rope ++ literal ++ (Rope.hole ~prov hole) in
            run stack [] { b with rope } (source acc)
      end
    | "seq" -> run (XmlStack.push stack) seq b (source acc)
    | "let" ->
      let seq = rtrim seq in
      let name = get_attr "let" attrs "name" in
      let letb = { b with rope = empty } in
      let acc, letb = run [0,[]] [] letb (source acc) in
      let b = { b with bindings = declare [name] letb.rope bindings } in
      run (XmlStack.save_bindings bindings stack) seq b (source acc)
    | el ->
      raise (Error (`Unknown_tag el))
  ) in
  fun acc ->
    let bindings = Bindings.empty in
    let blueprint = { rope = Rope.empty; bindings } in
    run [1,[]] [] blueprint (source acc)

let xml_source xml_input =
  xml_input, if Xmlm.eoi xml_input then None else Some (Xmlm.input xml_input)

let default_hole = Hole.(function
  | { default = Some (Default rope) }    -> Some rope
  | { default = Some (Typed (_, rope)) } -> Some rope
  | { default = None }                   -> None
)

let bind_hole bindings hole =
  match Bindings.get bindings [Hole.name hole] with
  | Some t -> Some t
  | None -> default_hole hole

let bind ?(incomplete=false) ~sink acc bindings rope =
  let bind_hole = bind_hole bindings in
  let patch prov acc hole =
    acc, match bind_hole hole with
    | None when incomplete -> Rope.hole prov hole
    | None -> raise (Error (`Empty_hole (Hole.name hole)))
    | Some t -> t
  in
  Rope.to_stream ~patch ~sink acc rope
