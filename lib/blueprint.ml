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

(* This is a simple templating system based on XML ropes with named
   holes. *)

module Prov = struct
  type loc = int * int
  type src = Stream of string | File of string
  type t = {
    src  : src;
    loc  : loc option;
    incl : (string * t) option;
  }

  let with_loc prov loc = { prov with loc = Some loc }

  let with_incl prov name parent = { prov with incl = Some (name, parent) }

  let rec append_incl prov name parent = match prov.incl with
    | None -> { prov with incl = Some (name, parent) }
    | Some (incl_name,incl) ->
      { prov with incl = Some (incl_name, append_incl incl name parent) }

  let string_of_loc (l,c) = Printf.sprintf "line %d, column %d" l c

  let string_of_src = function
    | Stream s -> Printf.sprintf "stream '%s'" s
    | File f -> Printf.sprintf "file '%s'" f

  let rec to_string = function
    | { src; loc = Some loc; incl = Some (incl,parent) } ->
      Printf.sprintf "from %s, %s\nincluded as '%s' %s"
        (string_of_src src) (string_of_loc loc) incl (to_string parent)
    | { src; loc = None; incl = Some (incl,parent) } ->
      Printf.sprintf "from %s\nincluded as '%s' %s"
        (string_of_src src) incl (to_string parent)
    | { src; loc = Some loc; incl = None } ->
      Printf.sprintf "from %s, %s" (string_of_src src) (string_of_loc loc)
    | { src; loc = None; incl = None } ->
      Printf.sprintf "from %s" (string_of_src src)
end

type signal = Prov.loc * Xmlm.signal

type error = [
  | `Empty_hole of Prov.t option * string
  | `Bad_ident of Prov.loc * string
  | `Unknown_tag of Prov.loc * string
  | `Missing_attribute of Prov.loc option * string * string
  | `Floating_attr of Prov.loc * string
  | `Data_after_root of Prov.loc option
  | `Element_after_root of Prov.loc option
]

exception Error of error

let xmlns = "https://opam.ocaml.org/packages/blueprint/xmlns/0/#"

let xmlns_map_default ns = if ns = xmlns then Some "t" else None

let error_message : error -> string = Printf.(function
  | `Empty_hole (None, name) ->
    sprintf "No value for hole named '%s'" name
  | `Empty_hole (Some prov, name) ->
    sprintf "No value for hole named '%s'\n%s" name (Prov.to_string prov)
  | `Bad_ident (loc, name) ->
    sprintf "%s: the identifier '%s' is invalid" (Prov.string_of_loc loc) name
  | `Unknown_tag (loc, tag) ->
    sprintf "%s: unknown tag '%s'" (Prov.string_of_loc loc) tag
  | `Missing_attribute (None,tag,attr) ->
    sprintf "Tag '%s' is missing attribute '%s'" tag attr
  | `Missing_attribute (Some loc,tag,attr) ->
    sprintf "%s: tag '%s' is missing attribute '%s'"
      (String.capitalize (Prov.string_of_loc loc)) tag attr
  | `Floating_attr (loc, attr) ->
    sprintf "%s: the attribute '%s' is not attached to a start tag"
      (Prov.string_of_loc loc) attr
  | `Data_after_root None -> "Data are not allowed after the root element"
  | `Data_after_root (Some loc) ->
    sprintf "%s: data are not allowed after the root element"
      (Prov.string_of_loc loc)
  | `Element_after_root None -> "Tags are not allowed after the root element"
  | `Element_after_root (Some loc) ->
    sprintf "%s: tags are not allowed after the root element"
      (Prov.string_of_loc loc)
)

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

  let of_string loc ctxt s =
    let ident = Stringext.split ~on:'.' s in
    if List.for_all is_valid ident
    then match ident with
      | ""::rest -> ctxt @ rest
      | _ -> ident
    else raise (Error (`Bad_ident (loc, s)))

  let compare = compare_string_list
end

module StringMap = Map.Make(String)

module Scope = struct
  type 'rope t = {
    map : 'rope obj StringMap.t;
    gen : 'rope gen option;
  }
  and 'rope obj = {
    rope : 'rope option;
    children : 'rope t;
  }
  and 'rope gen = {
    lookup : Ident.t -> 'rope obj option;
    pop    : unit -> ((string * 'rope obj) * 'rope gen option) option;
    shadow : 'rope gen -> 'rope gen;
  }

  let empty = { map = StringMap.empty; gen = None; }
  let empty_obj = { rope = None; children = empty; }

  let template { rope } = rope
  let children { children } = children

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
        | Some agen -> Some (agen.shadow bgen)
  }

  let get bindings ident =
    let rec aux obj { map; gen } = function
      | [] -> obj
      | (name::rest) as ident ->
        match try Some (StringMap.find name map) with Not_found -> None with
        | Some obj -> aux (Some obj) obj.children rest
        | None -> match gen with
          | None -> None
          | Some gen -> gen.lookup ident
    in
    aux None bindings ident

  let find bindings name =
    try
      get bindings (Ident.of_string (-1,-1) [] name)
    with Error (`Bad_ident _) -> None

  let pop bindings =
    try
      let (k, _v) as pair = StringMap.min_binding bindings.map in
      Some (pair, { bindings with map = StringMap.remove k bindings.map })
    with Not_found -> match bindings.gen with
      | None -> None
      | Some gen -> match gen.pop () with
        | None -> None
        | Some (pair, gen) ->
          Some (pair, { bindings with gen })

  let get_obj bindings ident = match get bindings ident with
    | Some obj -> obj
    | None -> empty_obj

  let rec apply bindings f = function
    | [] -> bindings
    | [name] as ident ->
      let map = match f bindings ident with
        | None -> StringMap.remove name bindings.map
        | Some obj -> StringMap.add name obj bindings.map
      in
      { bindings with map }
    | name::rest ->
      let obj = get_obj bindings [name] in
      let obj = { obj with children = apply obj.children f rest } in
      let map = StringMap.add name obj bindings.map in
      { bindings with map }

  let put bindings ident obj =
    apply bindings (fun _ _ -> Some obj) ident

  let declare bindings ident rope =
    apply bindings (fun bindings ident ->
      let obj = get_obj bindings ident in
      Some { obj with rope = Some rope }
    ) ident
end

module Env = struct
  type 'rope t = {
    scope : 'rope Scope.t;
    expanded : ('rope,int) Hashtbl.t
  }

  let create scope = { scope; expanded = Hashtbl.create 8 }

  let expand env rope =
    let expanded = Hashtbl.copy env.expanded in
    let quota = try Hashtbl.find expanded rope with Not_found -> 1 in
    Hashtbl.replace expanded rope (quota - 1);
    { env with expanded }

  let is_expandable env rope =
    try
      let quota = Hashtbl.find env.expanded rope in
      quota > 0
    with Not_found -> true

  let set_expansion env rope i =
    let expanded = Hashtbl.copy env.expanded in
    Hashtbl.replace expanded rope i;
    { env with expanded }
end

module Hole = struct
  type 'value value =
    | Subtree of 'value option

  type ('rope, 'value) t = {
    name : Ident.t;
    value : 'value value;
    env : 'rope Scope.t;
  }

  let name { name } = name

  let named name env = { name; value = Subtree None; env; }
  let valued name value env = { name; value = Subtree (Some value); env; }
end

let get_rope bindings ident = Scope.(match get bindings ident with
  | Some { rope = Some rope } -> Some rope
  | Some { rope = None } | None -> None
)

let empty_seq = [ `El_start ((xmlns,"seq"),[]); `El_end ]

module type PATCH = sig
  module Rope : XmlRope.S

  val patch_hole : partial:bool -> Rope.t Env.t Rope.patch
end

module rec Template : XmlRope.TEMPLATE
  with type hole = (Patch.Rope.t, Rope.t) Hole.t
   and type prov = Prov.t
= struct
  type hole = (Patch.Rope.t, Rope.t) Hole.t
  type prov = Prov.t

  (* TODO: closure bindings? *)
  let signals_of_hole ~prov hole = Hole.(
    match hole.value with
    | Subtree default ->
      let ident = String.concat "." hole.name in
      (`El_start ((xmlns,"insert"),[("","name"),ident]))::
      begin match default with
        | None -> [ `El_end ]
        | Some rope -> Rope.(
          let patch = Patch.patch_hole ~partial:true in
          let default = to_list ~patch (Env.create hole.env) rope in
          match default with [] -> empty_seq | _ -> default
        ) @ [ `El_end ]
      end
  )

end

and Rope : XmlRope.S
  with type hole = Template.hole
   and type prov = Template.prov
  = XmlRope.Make(Template)

and Patch : PATCH
  with type Rope.t = Rope.t
   and type Rope.hole = Template.hole
   and type Rope.prov = Template.prov
= struct
  module Rope = Rope

  let rec patch_hole ~partial : Rope.t Env.t Rope.patch = Hole.(
    fun env prov hole ->
      let { Env.scope } = env in
      match hole with
      | { name; value = Subtree default } ->
        let rope_opt = get_rope scope name in
        let scope = Scope.shadow hole.env scope in
        let env = { env with Env.scope } in
        match rope_opt with
        | Some t ->
          if not (Env.is_expandable env t)
          then None
          else
            let env = Env.expand env t in
            let t = Rope.map_prov (fun parent ->
              (* TODO: this potential replacement seems dodgy *)
              Prov.with_incl parent (String.concat "." name) prov
            ) t in
            Some (env, t)
        | None -> match default with
          | None when partial -> None
          | None ->
            raise (Error (`Empty_hole (Some prov, String.concat "." name)))
          | Some t ->
            if not (Env.is_expandable env t)
            then None
            else
              let env = Env.expand env t in
              if partial
              then
                let patch_hole = patch_hole ~partial in
                Some (env, Rope.make_hole ~prov {
                  hole with value = Subtree (Some (Rope.patch patch_hole env t))
                })
              else Some (env, t)
  )
end

let patch_hole = Patch.patch_hole

type t = Rope.t Scope.obj

let default_rope = function None -> Rope.empty | Some rope -> rope

let get_attr loc tag attrs attr =
  try List.assoc ("",attr) attrs
  with Not_found -> raise (Error (`Missing_attribute (Some loc, tag, attr)))

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

let is_ws s =
  let len = String.length s in
  let rec loop i =
    if i < len
    then match String.get s i with
      | ' ' | '\t' | '\n' -> loop (i+1)
      | _ -> false
    else true
  in
  loop 0

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
  xml_input,
  let pos = Xmlm.pos xml_input in
  if Xmlm.eoi xml_input
  then None
  else Some (pos, Xmlm.input xml_input)

let bind ?(partial=false) ~sink out bindings rope =
  let patch = patch_hole ~partial in
  Rope.to_stream ~patch ~sink (Env.create bindings) out rope

let xml_sink ?(partial=false) () =
  let depth = ref 0 in (* TODO: this isn't very nice... *)
  let after_root = ref false in
  fun prov out s -> List.iter (function
    | `Data s when !depth = 0 && is_ws s -> ()
    | (`Data _ | `Dtd _) as signal ->
      if !after_root
      then raise (Error (`Data_after_root prov.Prov.loc));
      Xmlm.output out signal
    | (`El_start ((ns,tag),attrs))
      when not partial && ns = xmlns ->
      begin try let name = List.assoc ("","name") attrs in
          raise (Error (`Empty_hole (Some prov, name)))
        with Not_found ->
          let { Prov.loc } = prov in
          raise (Error (`Missing_attribute (loc, tag, "name")))
      end
    | (`El_start _) as signal ->
      if !after_root
      then raise (Error (`Element_after_root prov.Prov.loc));
      incr depth;
      Xmlm.output out signal
    | `El_end ->
      decr depth;
      if !depth = 0 then after_root := true;
      Xmlm.output out `El_end
  ) s; out

let buffer_sink ?partial buffer =
  let ns_prefix = xmlns_map_default in
  let xml_out =
    Xmlm.make_output ~decl:false ~nl:true ~ns_prefix (`Buffer buffer)
  in
  let sink = xml_sink ?partial () in
  fun prov () s -> ignore (sink prov xml_out s)

let bind_to_output ?partial out bindings rope =
  let buffer = Buffer.create 1024 in
  let sink = buffer_sink ?partial buffer in
  bind ?partial ~sink () bindings rope;
  Buffer.output_buffer out buffer

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
    | acc, Some (loc, el) -> match el with
      | `El_start ((ns,el),attrs) when ns=xmlns ->
        handle stack seq ctxt b acc attrs loc el
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

  and handle stack seq ctxt ({ rope; children } as b) acc attrs loc = Rope.(
    function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      let name = Ident.of_string loc ctxt (get_attr loc "insert" attrs "name") in
      begin match get_rope children name with
        | None -> add_hole stack seq ctxt b acc name literal loc
        | Some template ->
          let prov = Prov.with_loc prov loc in
          let template = Rope.map_prov (fun parent ->
            let name = String.concat "." name in
            Prov.append_incl parent name prov
          ) template in
          let patch_hole = patch_hole ~partial:true in
          let env = Env.create children in
          let template = patch patch_hole env template in
          (* Now, we throw away any default value. *)
          let default = { b with rope = None } in
          let acc, _default = run [0,[]] [] ctxt default (source acc) in
          let rope = rope +? literal ++ template in
          run stack [] ctxt (with_rope b rope) (source acc)
      end

    | "attr" ->
      let name = get_attr loc "attr" attrs "name" in
      let content = { b with rope = None } in
      let acc, content = run [0,[]] [] ctxt content (source acc) in
      let rope = match rtrim_all seq with
        | (`El_start (tag,attrs))::r ->
          let literal = of_list ~prov (List.rev r) in
          let attr = (("",name), default_rope content.rope) in
          rope +? literal ++ (make_attrs ~prov (tag,attrs) [attr])
        | _::_ -> raise (Error (`Floating_attr (loc, name)))
        | [] -> match rope with
          | Some rope when in_attrs rope ->
            let attr = (("",name), default_rope content.rope) in
            with_attr rope attr
          | Some _ | None -> raise (Error (`Floating_attr (loc, name)))
      in
      run stack [] ctxt (with_rope b rope) (source acc)

    | "seq" -> run (XmlStack.push stack) seq ctxt b (source acc)

    | "let" ->
      let seq = rtrim seq in
      let name = Ident.of_string loc ctxt (get_attr loc "let" attrs "name") in
      let acc, letb = run [0,[]] [] name { b with rope = None } (source acc) in
      let obj = Scope.({ (get_obj letb.children name) with rope = letb.rope }) in
      let b = { b with children = Scope.put children name obj } in
      run (XmlStack.save_bindings children stack) seq ctxt b (source acc)

    | el ->
      raise (Error (`Unknown_tag (loc, el)))
  )

  and add_hole stack seq ctxt ({ rope; children } as b) acc name literal loc =
    let open Rope in
    let prov = Prov.with_loc prov loc in
    match source acc with
    | (acc, (None | Some (_,`El_end))) as c ->
      let hole = Hole.named name children in
      let rope = rope +? literal ++ (make_hole ~prov hole) in
      run (XmlStack.push stack) [] ctxt (with_rope b rope) c
    | (acc, _signal_opt) as c ->
      let default = { b with rope = None } in
      let acc, default = run [0,[]] [] ctxt default c in
      let default_rope = default_rope default.rope in
      let hole = Hole.valued name default_rope children in
      let rope = rope +? literal ++ (make_hole ~prov hole) in
      run stack [] ctxt (with_rope b rope) (source acc)
  in
  fun acc ->
    run [1,[]] [] [] empty_blueprint (source acc)
