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
  | `Bad_ident of string
  | `Unknown_tag of string
  | `Missing_attribute of string * string
  | `Floating_attr of string
  | `Data_after_root
  | `Element_after_root
]

exception Error of error

let xmlns = "https://opam.ocaml.org/packages/blueprint/xmlns/0/#"

let xmlns_map_default ns = if ns = xmlns then Some "t" else None

let error_message : error -> string = function
  | `Empty_hole name -> "No value for hole named '"^name^"'"
  | `Bad_ident name -> "The identifier '"^name^"' is invalid"
  | `Unknown_tag tag -> "Unknown tag '"^tag^"'"
  | `Missing_attribute (tag,attr) ->
    "Tag '"^tag^"' is missing attribute '"^attr^"'"
  | `Floating_attr attr ->
    "The attribute '"^attr^"' is not attached to a start tag"
  | `Data_after_root -> "Data are not allowed after the root element"
  | `Element_after_root -> "Tags are not allowed after the root element"

let rec compare_string_list sl sl' = match sl, sl' with
  | [], [] -> 0
  | _::_, [] -> 1
  | [], _::_ -> -1
  | h::t, h'::t' -> match String.compare h h' with
    | 0 -> compare t t'
    | x -> x

module Ident = struct
  type t = string list

  let is_valid s =
    try
      String.iter (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> ()
        | _ -> raise Not_found
      ) s;
      true
    with Not_found -> false

  let of_string ctxt s =
    let ident = Stringext.split ~on:'.' s in
    if List.for_all is_valid ident
    then match ident with
      | ""::rest -> ctxt @ rest
      | _ -> ident
    else raise (Error (`Bad_ident s))

  let compare = compare_string_list
end

module StringMap = Map.Make(String)

module Scope = struct

  type 'rope t = {
    map : 'rope obj StringMap.t;
    gen : (Ident.t -> 'rope obj option) option;
  }
  and 'rope obj = {
    rope : 'rope option;
    children : 'rope t;
  }

  let empty = { map = StringMap.empty; gen = None; }
  let empty_obj = { rope = None; children = empty }

  let template { rope } = rope
  let scope { children } = children

  let with_rope obj rope = { obj with rope = Some rope }

  let rec shadow a b = {
    map = StringMap.merge (fun _name ao bo -> match ao, bo with
      | None, None -> None
      | Some _, None -> ao
      | None, Some _ -> bo
      | Some a, Some b -> Some {
        rope = a.rope;
        children = shadow a.children b.children;
      }
    ) a.map b.map;
    gen = match b.gen with
      | None -> a.gen
      | Some bgen -> match a.gen with
        | None -> Some bgen
        | Some agen -> Some (fun ident ->
          match agen ident with None -> bgen ident | (Some _) as o -> o
        )
  }

  let get bindings ident =
    let rec aux obj { map; gen } = function
      | [] -> obj
      | (name::rest) as ident ->
        match try Some (StringMap.find name map) with Not_found -> None with
        | Some obj -> aux (Some obj) obj.children rest
        | None -> match gen with
          | None -> None
          | Some gen -> gen ident
    in
    aux None bindings ident

  let get_obj bindings ident = match get bindings ident with
    | Some obj -> obj
    | None -> empty_obj

  let rec apply bindings f = function
    | [] -> bindings
    | [name] as ident ->
      let obj = f bindings ident in
      let map = StringMap.add name obj bindings.map in
      { bindings with map }
    | name::rest ->
      let obj = get_obj bindings [name] in
      let obj = { obj with children = apply obj.children f rest } in
      let map = StringMap.add name obj bindings.map in
      { bindings with map }

  let put bindings ident obj =
    apply bindings (fun _ _ -> obj) ident

  let declare bindings ident rope =
    apply bindings (fun bindings ident ->
      let obj = get_obj bindings ident in
      { obj with rope = Some rope }
    ) ident

end

module Hole = struct
  type ('rope, 'value) t = {
    name : Ident.t;
    default : 'value option;
    env : 'rope Scope.t;
  }

  let name { name } = name

  let named name env = { name; default = None; env; }
  let valued name value env = { name; default = Some value; env; }
end

(* TODO: What about lexical info? values are assumed to be static per
   source *)
type prov =
  | Stream of string
  | File of string
type prov' = prov

let get_rope bindings ident = Scope.(match get bindings ident with
  | Some { rope = Some rope } -> Some rope
  | Some { rope = None } | None -> None
)

let bind_hole bindings hole = match get_rope bindings (Hole.name hole) with
  | Some t -> Some t
  | None -> hole.Hole.default

let rec patch_hole ~partial make_hole patch env prov hole = Hole.(
  match get_rope env (name hole) with
  | Some t -> (Scope.shadow hole.env env, t)
  | None -> match hole.default with
    | None when partial -> env, make_hole ~prov hole
    | None -> raise (Error (`Empty_hole (String.concat "." (name hole))))
    | Some t ->
      let env = Scope.shadow hole.env env in
      let patch_hole = patch_hole ~partial make_hole patch in
      if partial
      then (env, make_hole ~prov {
        hole with default = Some (patch patch_hole env t)
      })
      else (env, t)
)

let empty_seq = [ `El_start ((xmlns,"seq"),[]); `El_end ]

module rec Template :
  XmlRope.TEMPLATE with type hole = (Rope.t, Rope.t) Hole.t
                    and type prov = prov
= struct
  type hole = (Rope.t, Rope.t) Hole.t
  type prov = prov'

  (* TODO: closure bindings? *)
  let signals_of_hole _prov hole = Hole.(
    let ident = String.concat "." hole.name in
    (`El_start ((xmlns,"insert"),[("","name"),ident]))::
    match hole.default with
    | None -> [ `El_end ]
    | Some rope -> Rope.(
      let patch = patch_hole ~partial:true make_hole patch in
      let default = to_list ~patch hole.env rope in
      match default with [] -> empty_seq | _ -> default
    ) @ [ `El_end ]
  )
end
and Rope : XmlRope.S
  with type hole = Template.hole
   and type prov = Template.prov
  = XmlRope.Make(Template)

type t = Rope.t Scope.obj

let default_rope = function None -> Rope.empty | Some rope -> rope

let get_attr tag attrs attr =
  try List.assoc ("",attr) attrs
  with Not_found -> raise (Error (`Missing_attribute (tag, attr)))

module XmlStack = struct
  type frame = int * (int * Rope.t Scope.t) list
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

let rec rtrim_all = function
  | (`Data s)::r ->
    let len = String.length s in
    let rec last i =
      let i = i - 1 in
      if i < 0 then 0
      else match String.get s i with
        | ' ' | '\t' | '\n' -> last i
        | _ -> i + 1
    in
    if len > 0
    then match last len with
      | 0 -> rtrim_all r
      | i -> (`Data (String.sub s 0 i))::r
    else rtrim_all r
  | s -> s

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
    then match last len with
      | 0 -> r
      | i -> (`Data (String.sub s 0 i))::r
    else r
  | s -> s

let empty_blueprint = Scope.empty_obj

let xml_source xml_input =
  xml_input, if Xmlm.eoi xml_input then None else Some (Xmlm.input xml_input)

let bind ?(partial=false) ~sink out bindings rope =
  let patch = patch_hole ~partial Rope.make_hole Rope.patch in
  Rope.to_stream ~patch ~sink bindings out rope

(*
let string_of_rope rope =
  let sink _prov out s = List.iter (Xmlm.output out) s; out in
  let suppress = ref true in
  let buffer = Buffer.create 16 in
  let output byte =
    if not !suppress then Buffer.add_char buffer (char_of_int byte)
  in
  let ns_prefix = xmlns_map_default in
  let out = Xmlm.make_output ~decl:false ~ns_prefix (`Fun output) in
  Xmlm.output out (`Dtd None);
  Xmlm.output out (`El_start (("","x"),[]));
  Xmlm.output out (`Data "");
  suppress := false;
  let _out = bind ~partial:true ~sink out Bindings.empty rope in
  Buffer.contents buffer
*)

let of_stream ~prov ~source =
  let open Scope in
  let rec run stack seq ctxt ({ rope; children } as b) = function
    | acc, None ->
      acc, with_rope b Rope.(rope +? (of_list ~prov (List.rev seq)))
    | acc, Some el -> match el with
      | `El_start ((ns,el),attrs) when ns=xmlns ->
        handle stack seq ctxt b acc attrs el
      | `El_start el ->
        run (XmlStack.incr stack) ((`El_start el)::seq) ctxt b (source acc)
      | `El_end when XmlStack.consumep stack ->
        begin match XmlStack.pop stack with
          | [] ->
            acc, with_rope b Rope.(rope +? (of_list ~prov (List.rev seq)))
          | stack -> run stack seq ctxt b (source acc)
        end
      | `El_end ->
        let stack, children = XmlStack.decr children stack in
        run stack (`El_end::seq) ctxt { b with children } (source acc)
      | (`Dtd _ | `Data _) as signal ->
        run stack (signal::seq) ctxt b (source acc)

  and handle stack seq ctxt ({ rope; children } as b) acc attrs = Rope.(function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      let name = Ident.of_string ctxt (get_attr "insert" attrs "name") in
      begin match get_rope children name with
        | None -> add_hole stack seq ctxt b acc name literal
        | Some template ->
          let patch_hole = patch_hole ~partial:true make_hole patch in
          let template = patch patch_hole children template in
          (*let template = patch (fun env prov hole ->
            match bind_hole env hole with
            | None -> (env, Rope.make_hole ~prov hole)
            | Some t -> (shadow hole.Hole.env env, t)
          ) children template in
          *)
          (* Now, we throw away any default value. *)
          let default = { b with rope = None } in
          let acc, _default = run [0,[]] [] ctxt default (source acc) in
          let rope = rope +? literal ++ template in
          run stack [] ctxt (with_rope b rope) (source acc)
      end

    | "attr" ->
      let name = get_attr "attr" attrs "name" in
      let content = { b with rope = None } in
      let acc, content = run [0,[]] [] ctxt content (source acc) in
      let rope = match rtrim_all seq with
        | (`El_start (tag,attrs))::r ->
          let literal = of_list ~prov (List.rev r) in
          let attr = (("",name), default_rope content.rope) in
          rope +? literal ++ (make_attrs ~prov (tag,attrs) [attr])
        | _::_ -> raise (Error (`Floating_attr name))
        | [] -> match rope with
          | Some rope when in_attrs rope ->
            let attr = (("",name), default_rope content.rope) in
            with_attr rope attr
          | Some _ | None -> raise (Error (`Floating_attr name))
      in
      run stack [] ctxt (with_rope b rope) (source acc)

    | "seq" -> run (XmlStack.push stack) seq ctxt b (source acc)

    | "let" ->
      let seq = rtrim seq in
      let name = Ident.of_string ctxt (get_attr "let" attrs "name") in
      let acc, letb = run [0,[]] [] name { b with rope = None } (source acc) in
      let obj = Scope.({ (get_obj letb.children name) with rope = letb.rope }) in
      let b = { b with children = Scope.put children name obj } in
      run (XmlStack.save_bindings children stack) seq ctxt b (source acc)

    | el ->
      raise (Error (`Unknown_tag el))
  )

  and add_hole stack seq ctxt ({ rope; children } as b) acc name literal = Rope.(
    match source acc with
    | (acc, (None | Some `El_end)) as c ->
      let hole = Hole.named name children in
      let rope = rope +? literal ++ (make_hole ~prov hole) in
      run (XmlStack.push stack) [] ctxt (with_rope b rope) c
    | acc, signal_opt ->
      let default = { b with rope = None } in
      let acc, default = run [0,[]] [] ctxt default (acc, signal_opt) in
      let default_rope = default_rope default.rope in
      let hole = Hole.valued name default_rope children in
      let rope = rope +? literal ++ (make_hole ~prov hole) in
      run stack [] ctxt (with_rope b rope) (source acc)
  ) in
  fun acc ->
    run [1,[]] [] [] empty_blueprint (source acc)
