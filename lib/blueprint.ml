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

module Debug = struct
  let scope = 0x01
  let get = 0x02
  let expand = 0x04
  let v = 0 (*scope lor get lor expand*)
end
let log tag s = if Debug.v land tag <> 0 then print_endline s

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

type binding_error = [
  | `Empty_hole of Prov.t option * string
  | `Dangling_link of Prov.t * string * string
    (* only occurs during expansion right now *)
]

type expansion_error = [
  | binding_error
  | `Disallowed_expansion of Prov.t * string
]

type error = [
  | expansion_error
  | `Bad_ident of Prov.loc * string
  | `Unknown_tag of Prov.loc * string
  | `Missing_attribute of Prov.loc option * string * string
  | `Floating_attr of Prov.loc * string
  | `Floating_else of Prov.loc
  | `Data_after_root of Prov.loc option
  | `Element_after_root of Prov.loc option
]

type ('ok,'err) result = Ok of 'ok | Err of 'err

exception Error of error

let xmlns = "https://opam.ocaml.org/packages/blueprint/xmlns/0/#"

let xmlns_map_default ns = if ns = xmlns then Some "t" else None

let rec string_of_path acc = function
  | [] -> acc
  | [s] -> acc^s
  | h::t -> string_of_path (acc^h^" -> ") t

let error_message : error -> string = Printf.(function
  | `Empty_hole (None, name) ->
    sprintf "No value for hole named '%s'" name
  | `Empty_hole (Some prov, name) ->
    sprintf "No value for hole named '%s'\n%s" name (Prov.to_string prov)
  | `Dangling_link (prov, dangling, expanding) ->
    sprintf "Dangling link '%s' prevented expansion of '%s'\n%s"
      dangling expanding (Prov.to_string prov)
  | `Disallowed_expansion (prov, name) ->
    sprintf "Disallowed expansion of '%s'\n%s"
      name (Prov.to_string prov)
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
  | `Floating_else loc ->
    sprintf "%s: the else tag is not attached to an if tag"
      (Prov.string_of_loc loc)
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
  type absolute = [ `Absolute ]
  type relative = [ `Relative ]
  type any = [ absolute | relative ]
  type _ kind =
    | Absolute : [< any > `Absolute] kind
    | Relative : [< any > `Relative] kind
  type 'a t = {
    kind : 'a kind;
    ident : string list;
    loc : Prov.loc option;
  }
  type relative_t = relative t
  type absolute_t = absolute t
  type any_t = any t

  module Absolute = struct
    type t = absolute_t
    let compare { ident=a } { ident=b } = compare_string_list a b
  end

  let any : type k. k t -> any_t = function
    | { kind = Relative } as x -> x
    | { kind = Absolute } as x -> x

  let is_valid s =
    try
      String.iter (function
        | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> ()
        | _ -> raise Not_found
      ) s;
      true
    with Not_found -> false

  let resolve (type a) : absolute t -> a t -> absolute t = fun ctxt -> function
    | { kind = Absolute } as ident -> ident
    | { kind = Relative; ident; loc } ->
      let { ident = ctxt } = ctxt in
      { kind = Absolute; ident = ctxt @ ident; loc }

  let append (type a) : relative t -> a t -> a t = fun ctxt -> function
    | { kind = Absolute } as ident -> ident
    | { kind = Relative; ident; loc } ->
      let { ident = ctxt } = ctxt in
      { kind = Relative; ident = ctxt @ ident; loc }

  let root = { kind = Absolute; ident = []; loc = None }
  let here = { kind = Relative; ident = []; loc = None }

  let segment segment = { kind = Relative; ident = [segment]; loc = None }

  (* Though identifiers are not zippers, this returns both parts of
     the original identifier. *)
  let prev : 'a. 'a t -> ('a t * relative_t) option = function
    | { ident = [] } -> None
    | { ident } as x ->
      let rident = List.rev ident in
      let last = List.hd rident in
      let rest = List.(rev (tl rident)) in
      Some ({ x with ident = rest }, { x with kind = Relative; ident = [last] })

  let next : 'a. 'a t -> ('a t * relative_t) option = function
    | { ident = [] } -> None
    | { kind; ident = x::xs } as i ->
      Some ({ i with kind; ident = [x] },
            { i with kind = Relative; ident = xs })

  let extend : 'a. 'a t -> string -> 'a t = fun ident segment ->
    { ident with ident = ident.ident@[segment] }

  let of_string loc s =
    let ident = Stringext.split ~on:'.' s in
    if List.for_all is_valid ident
    then match ident with
      | ""::ident -> { kind = Relative; ident; loc = Some loc }
      | _ -> { kind = Absolute; ident; loc = Some loc }
    else raise (Error (`Bad_ident (loc, s)))

  let to_string = function
    | { kind = Absolute; ident } -> String.concat "." ident
    | { kind = Relative; ident } -> "."^String.concat "." ident

  let compare a b =
    match a, b with
    | { kind = Relative; ident=a }, { kind = Relative; ident=b }
    | { kind = Absolute; ident=a }, { kind = Absolute; ident=b } ->
      compare_string_list a b
    | { kind = Relative }, { kind = Absolute } -> -1
    | { kind = Absolute }, { kind = Relative } ->  1

  let is_prefix prefix ident =
    let rec aux = function
      | x::xs, y::ys when x = y -> aux (xs,ys)
      | [], _ -> true
      | _, _ -> false
    in
    if prefix.kind = ident.kind
    then aux (prefix.ident,ident.ident)
    else false
end

module Condition = struct
  type t =
    | Exists of Ident.any_t
end

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

module IdentSet = Set.Make(Ident.Absolute)

module IntSet = Set.Make(struct
    type t = int

    let compare (x: t) (y: t) = compare x y
  end)

module Scope = struct
  type 'rope t = {
    rope : 'rope option Lazy.t;
    children : 'rope t StringMap.t Lazy.t;
    parent : (string * 'rope t) option;
    id : Ident.absolute_t;
    serial : int;
    filter : IntSet.t;
    prefilter : IntSet.t;
  }

  let issue = ref 0

  let empty () =
    let rope = Lazy.from_val None in
    let children = Lazy.from_val StringMap.empty in
    let parent = None in
    let id = Ident.root in
    let serial = !issue in
    let filter = IntSet.empty in
    let prefilter = IntSet.empty in
    incr issue;
    { rope; children; parent; id; serial; filter; prefilter }

  let template { rope } = rope
  let children { children } = children

  let serials { children = lazy children } =
    StringMap.fold (fun _ { serial } -> IntSet.add serial) children IntSet.empty

  let with_maybe_rope obj rope =
    let serial = !issue in
    incr issue;
    { obj with rope; serial }

  let with_rope obj rope =
    with_maybe_rope obj (Lazy.from_fun (fun () -> Some rope))

  let up = function
    | { parent = None } as n -> n
    | { parent = Some (name, p) } as n ->
      let children = Lazy.from_fun (fun () ->
        let lazy children = p.children in
        StringMap.add name n children
      ) in
      { p with children }

  let rec to_root = function
    | { parent = None } as n -> n
    | n -> to_root (up n)

  let fold_up f z scope =
    let rec aux acc = function
      | { parent = None } -> acc
      | { parent = Some (name,p) } as n ->
        aux (f acc n name p) (up n)
    in
    aux z scope

  let path scope =
    let ident = fold_up (fun ident _ name _ -> name::ident) [] scope in
    Ident.({ kind = Absolute; ident; loc = None })

  let to_string scope =
    let path = path scope in
    let scope = to_root scope in
    let open Printf in
    let buf = Buffer.create 64 in
    let rec print_obj p indent name scope =
      Buffer.add_string buf (sprintf "\n%s%s" indent name);
      print (Ident.extend p name) (indent^"  ") scope
    and print p indent { rope; children = lazy children; id; serial } =
      let cursor = if p = path then "*" else "" in
      let has_rope = match rope with
        | lazy (Some _) -> "+"
        | lazy None -> ""
      in
      let id = Ident.(to_string (any id)) in
      Buffer.add_string buf (sprintf "%s%s(%s,%d)" has_rope cursor id serial);
      StringMap.iter (print_obj p indent) children;
      if StringMap.cardinal children = 0 then Buffer.add_string buf "\n"
    in
    print Ident.root "  " scope;
    Buffer.contents buf

  (* The small step implementation for get; preserves zipper parent invariants *)
  let rec down ({ children = lazy children } as scope) name =
    try
      let c = StringMap.find name children in
      Some { c with parent = Some (name, scope) }
    with Not_found -> None
  and get_
    : 'a. (_ -> 'a) -> (_ -> _ -> 'a) -> _ -> Ident.any_t -> 'a =
    fun some missing scope ident ->
      let rec aux scope : Ident.relative_t -> _ = function
        | { Ident.ident = [] } -> some scope
        | { Ident.ident = name::rest } as ident ->
          match down scope name with
          | Some scope ->
            let ident = { ident with Ident.ident = rest } in
            aux scope ident
          | None -> missing scope ident
      in
      Ident.(match ident with
        | { kind = Absolute } ->
          let scope = to_root scope in
          let r = aux scope { ident with kind = Relative } in
          (*log Debug.get ("returning "^(Ident.to_string ident));*)
          r
        | { kind = Relative } as ident ->
          let r = aux scope ident in
          (*log Debug.get ("returning "^(Ident.to_string ident));*)
          r
      )
  and get scope ident =
    get_ (fun x -> Some x) (fun _ _ -> None) scope ident

  let find scope name =
    try
      get scope (Ident.of_string (-1,-1) name)
    with Error (`Bad_ident _) -> None

  let merge_lazy_option a b = match a with
    | lazy (Some _) -> a
    | lazy None -> b

  let merge_lazy_map c a b = Lazy.from_fun (fun () ->
    StringMap.merge (fun _name a b ->
      match a, b with
      | None, None -> None
      | Some _, None -> a
      | None, Some _ -> b
      | Some a, Some b -> Some (c a b)
    ) (Lazy.force a) (Lazy.force b)
  )

  let rec overlay a b = {
    rope = merge_lazy_option a.rope b.rope;
    children = merge_lazy_map overlay a.children b.children;
    parent = b.parent;
    id = b.id;
    serial = (let serial = !issue in incr issue; serial);
    filter = IntSet.union a.filter b.filter;
    prefilter = IntSet.union a.prefilter b.prefilter;
  }

  let rec rebind a b =
    let rope = merge_lazy_option a.rope b.rope in
    let lazy acs = a.children in
    let lazy bcs = b.children in
    let child_in_b name { serial } =
      try let bc = StringMap.find name bcs in serial = bc.serial
      with Not_found -> false
    in
    let children = Lazy.from_fun (fun () -> StringMap.(fold add acs bcs)) in
    let serial =
      (* This is intentionally address equality because ropes contain
         lazy values in closure scopes. If the ropes are different
         objects, even if they are structurally equal, it is OK to
         treat them as distinct. *)
      if rope == b.rope && StringMap.for_all child_in_b acs
      then b.serial
      else (let serial = !issue in incr issue; serial)
    in
    {
      rope; children; serial;
      parent = b.parent; id = b.id;
      filter = IntSet.union a.filter b.filter;
      prefilter = IntSet.union a.prefilter b.prefilter;
    }

  let rec synthesize scope = function
    | { Ident.ident = [] } -> scope
    | { Ident.ident = name::rest } as ident ->
      let ident = { ident with Ident.ident = rest } in
      let parent = Some (name, scope) in
      let id = Ident.(extend scope.id name) in
      synthesize { (empty ()) with parent; id } ident

  let rec get_obj scope ident =
    get_ (fun x -> x) synthesize scope ident

  let rec find_tail scope = function
    | { Ident.ident = [] } -> scope, Ident.here
    | { Ident.ident = name::rest } as ident ->
      match down scope name with
      | Some scope -> find_tail scope { ident with Ident.ident = rest }
      | None -> scope, ident

  let split scope = Ident.(function
    | { kind = Absolute } as ident ->
      let scope = to_root scope in
      find_tail scope { ident with kind = Relative }
    | { kind = Relative } as ident -> find_tail scope ident
  )

  let rec apply scope f : Ident.relative_t -> _ = function
    | { Ident.ident = [] } -> scope
    | { Ident.ident = [name] } ->
      let lazy children = scope.children in
      let children = match f name scope with
        | None -> StringMap.remove name children
        | Some obj -> StringMap.add name obj children
      in
      (* TODO: Should the f call and assignment be lazy? Space leak? *)
      let children = Lazy.from_fun (fun () -> children) in
      { scope with children }
    | { Ident.ident = name::rest } as ident ->
      let rec aux = function
        | Some scope -> up (apply scope f { ident with Ident.ident = rest })
        | None ->
          let r = match Ident.prev ident with
            | Some (parent, last) ->
              let parent_synth = get_obj scope (Ident.any parent) in
              apply parent_synth f last
            | None -> scope
          in
          List.fold_left (fun n _ -> up n) r rest
      in
      aux (down scope name)

  (* put replaces the obj at ident in scope *)
  let put scope ident obj =
    let set name parent =
      let id = Ident.(extend parent.id name) in
      let parent = Some (name, parent) in
      Some { obj with parent; id } (* TODO: should update num? *)
    in
    Ident.(match ident with
      | { kind = Absolute } ->
        let path = Ident.any (path scope) in
        let scope = apply (to_root scope) set { ident with kind = Relative } in
        get_obj scope path
      | { kind = Relative } as ident -> apply scope set ident
    )

  let get_rope scope ident = match get scope ident with
    | None -> Err `Not_found
    | Some { rope = lazy None } -> Err `No_template
    | Some { rope = lazy (Some rope) } -> Ok rope

  let link ?(filter=IntSet.empty) bind scope src_id dst_id =
    match get scope dst_id with
    | None -> Err (`Dangling_link dst_id)
    | Some dst ->
      let src = get_obj (to_root dst) Ident.(any (resolve scope.id src_id)) in
      let bound = bind dst src in
      let filter = IntSet.add dst.serial filter in
      let src_filter = IntSet.union src.filter filter in
      if Ident.is_prefix scope.id (path dst)
         || Ident.is_prefix (path scope) (path dst)
      then
        let { children = lazy bc } = bound in
        let filtered = ref false in
        let children = Lazy.from_val (StringMap.filter (fun _ v ->
          let p = IntSet.mem v.serial src_filter in
          if p then filtered := true;
          not p
        ) bc) in
        let serial =
          if !filtered
          then (let r = !issue in incr issue; r)
          else bound.serial
        in
        let prefilter = IntSet.union bound.prefilter (serials dst) in
        Ok ({ bound with children; serial; prefilter },
            filter)
      else Ok (bound, filter)

  let integrate_filters set scope =
    let p = path scope in
    IdentSet.fold (fun ident scope -> match get scope (Ident.any ident) with
      | None -> scope
      | Some scope ->
        let scope = {
          scope with prefilter = IntSet.empty; filter = scope.prefilter
        } in
        get_obj scope p
    ) set scope
end

module Closure = struct
  type 'rope frame =
    | Scope of Ident.any_t * 'rope Scope.t
    | Link of Ident.any_t * Ident.any_t
  type 'rope t = 'rope frame list

  let empty = []

  let apply_frame filter scope = function
    | Scope (id, v) ->
      let scope = Scope.(overlay v (get_obj scope id)) in
      Ok (scope, filter)
    | Link (src,dst) ->
      (*prerr_endline ("applying link closure to "
                     ^(Ident.to_string (Scope.path scope)));
      prerr_endline ("linking "
                     ^(Ident.to_string src)
                     ^" to "
                     ^(Ident.to_string dst));*)
      Scope.link ~filter Scope.rebind scope src dst

  let string_of_frame = function
    | Scope (id, scope) ->
      Printf.sprintf "%s : %s\n" (Ident.to_string id) (Scope.to_string scope)
    | Link (src,dst) ->
      Printf.sprintf "%s -> %s\n" (Ident.to_string src) (Ident.to_string dst)

  let apply filter closure scope =
    let p = Scope.path scope in
    let srcset = List.fold_left (fun s -> function
      | Link (src,_) -> IdentSet.add (Ident.resolve p src) s
      | Scope _ -> s
    ) IdentSet.empty closure in
    let rec aux scope filter = function
      | [] -> Ok (Scope.integrate_filters srcset scope, filter)
      | frame::rest -> match apply_frame filter scope frame with
        | Ok (scope,filter) ->
          let scope = Scope.get_obj scope (Ident.any p) in
          aux scope filter rest
        | Err _ as err -> err
    in
    aux scope filter (List.rev closure)

  let link src dst = Link (src, dst)

  let scope ident scope = Scope (ident, scope)

  let push closure frame = frame::closure

  let to_string closure =
    let buf = Buffer.create (List.length closure * 32) in
    List.iter (fun frame ->
      Buffer.add_string buf (string_of_frame frame)
    ) closure;
    Buffer.contents buf

end

module Hole = struct
  type ('rope, 'value) reference = {
    name : Ident.any_t;
    closure : 'rope Closure.t;
    default : 'value option;
    with_ : Ident.absolute_t option; (* only for reserialization *)
  }
  type ('rope, 'value) t =
    | Reference of ('rope, 'value) reference
    | Conditional of Condition.t * 'value * 'value option
    | Snoop of string * Ident.any_t option

  let name { name } = name

  let new_named ?with_ name closure =
    Reference { name; default = None; closure; with_; }
  let new_valued ?with_ name value closure =
    Reference { name; default = Some value; closure; with_; }

  let new_conditional cond body ?els () = Conditional (cond, body, els)

  let new_snoop loc label = function
    | None -> Snoop (label,None)
    | Some on -> Snoop (label, Some (Ident.of_string loc on))
end

let empty_seq = [ `El_start ((xmlns,"seq"),[]); `El_end ]

module Env = struct
  type 'rope t = {
    base : 'rope Scope.t;
    expanded : IntSet.t;
    linked : IntSet.t;
  }

  let create base = { base; expanded = IntSet.empty; linked = IntSet.empty; }
  let expand prov env name = match Scope.get env.base name with
    | None -> Err `Empty_hole
    | Some ({ Scope.serial } as base) ->
      if IntSet.mem serial env.expanded
      then Err (`Disallowed_expansion (prov, Ident.to_string name))
      else
        let expanded = IntSet.add serial env.expanded in
        Ok { env with base; expanded; }

end

module type PATCH = sig
  module Rope : XmlRope.S

  val patch_hole : partial:bool -> Rope.patch
end

module rec Patch : PATCH
  with module Rope = Rope
= struct
  module Rope = Rope

  let exists env ident =
    match Scope.get env.Env.base ident with Some _ -> true | None -> false

  let dump env partial prov hole s =
    let dump = Rope.of_list ~prov [
      `El_start (("","pre"),[]);
      `Data s;
      `El_end;
    ] in
    let trailer = Rope.(if partial then make_hole ~prov hole else empty) in
    Rope.(Replace (dump ++ trailer))

  let rec patch_hole ~partial : Rope.patch =
    fun env prov -> function
      | Hole.Snoop (label,None) as hole ->
        dump env partial prov hole (label^"\n"^(Scope.to_string env.Env.base))
      | Hole.Snoop (label,Some on) as hole ->
        begin match Scope.get env.Env.base on with
          | None ->
            dump env partial prov hole (label^" missing "^Ident.to_string on)
          | Some scope ->
            let scope = { scope with Scope.parent = None } in
            dump env partial prov hole (label^"\n"^(Scope.to_string scope))
        end
      | Hole.Conditional (Condition.Exists exid, body, els) -> Rope.(
        if exists env exid then Recurse (env, body)
        else if partial
        then
          let patch_hole = patch_hole ~partial in
          let body = patch patch_hole env body in
          let els = match els with
            | Some els -> Some (patch patch_hole env els)
            | None -> None
          in
          let cond = Hole.Conditional (Condition.Exists exid, body, els) in
          Replace (make_hole ~prov cond)
        else match els with
        | Some e -> Recurse (env, e)
        | None -> Replace empty
      )
      | Hole.Reference hole ->
        let { Env.base } = env in
        (*log Debug.scope "scope before overlay";*)
        (*log Debug.scope (Scope.to_string base);*)
        (*log Debug.scope "closure before overlay";*)
        (*log Debug.scope (Closure.to_string hole.Hole.closure);*)
        let closure = hole.Hole.closure in
        let name = Hole.name hole in
        (*log Debug.scope Ident.(
          "expanding "^(to_string name)^" with"
          );*)
        (*log Debug.scope (Scope.to_string base);*)
        let ident = Ident.resolve base.Scope.id name in
        let env, result =
          match Closure.apply env.Env.linked closure base with
          | Err (`Dangling_link dst_id) ->
            let dst = Ident.to_string dst_id in
            let name = Ident.to_string name in
            env, Err (`Dangling_link (prov, dst, name))
          | Ok (b,linked) ->
            let base = Scope.get_obj b (Ident.any base.Scope.id) in
            let env = { env with Env.base; linked } in
            match Scope.get_rope base name with
            | Err (`Not_found | `No_template) ->
              env,
              (Err (`Empty_hole (Some prov, Ident.(to_string (any ident)))))
            | Ok rope ->
              match Env.expand prov env name with
              | Err (`Disallowed_expansion _) as result ->
                env, result
              | Err `Empty_hole ->
                env,
                (Err (`Empty_hole (Some prov, Ident.(to_string (any ident)))))
              | Ok env -> env, Ok rope
        in
        match result with
        | Ok t ->
          let t = Rope.map_prov (fun parent ->
            (* TODO: this potential replacement seems dodgy *)
            Prov.with_incl parent Ident.(to_string (any ident)) prov
          ) t in
          Rope.Recurse (env, t)
        | Err err ->
          let { Hole.default } = hole in
          match default with
          | Some t -> begin match err with
            | #binding_error ->
              if partial
              then
                let patch_hole = patch_hole ~partial in
                Rope.Replace (Rope.make_hole ~prov (Hole.Reference {
                  hole with Hole.default = Some (Rope.patch patch_hole env t)
                }))
              else Rope.Recurse (env, t)
            | _ ->
              raise (Error err)
          end
          | None -> match err with
            | #binding_error when partial ->
              let name = Ident.any ident in
              Rope.Replace (Rope.make_hole ~prov (Hole.Reference {
                hole with Hole.name
              }))
            | _ -> raise (Error err)
end

and Template : XmlRope.TEMPLATE
  with type hole = (Rope.t, Rope.t) Hole.t
   and type prov = Prov.t
   and type env  = Rope.t Env.t
= struct
  type hole = (Rope.t, Rope.t) Hole.t
  type prov = Prov.t
  type env  = Rope.t Env.t

  let rec attrs_of_cond = Condition.(function
    | Exists exid -> [("","exists"),Ident.(to_string exid)]
  )

  (* TODO: closure bindings *)
  let signals_of_hole ~prov (env : env) = Hole.(function
    | Snoop (label,None) ->
      [`El_start ((xmlns,"snoop"),[("","label"),label]); `El_end]
    | Snoop (label,Some on) ->
      let on = Ident.to_string on in
      [`El_start ((xmlns,"snoop"),[("","label"),label; ("","on"),on]); `El_end]
    | Conditional (cond, body, els) ->
      let attrs = attrs_of_cond cond in
      let patch = Patch.patch_hole ~partial:true in
      (`El_start ((xmlns,"if"),attrs))::begin
        (* TODO: check the env ctxt *)
        Rope.to_list ~patch env body
      end @ [ `El_end ] @ (match els with None -> [] | Some e ->
        (`El_start ((xmlns,"else"),[]))::(Rope.to_list ~patch env e)@[`El_end])
    | Reference hole ->
      let base_path = env.Env.base.Scope.id in
      let ident = Ident.(resolve base_path hole.name) in
      let attrs = (("","name"),Ident.(to_string (any ident)))::(
        match hole.with_ with
        | None -> []
        | Some with_ -> [("","with"),Ident.(to_string (any with_))]
      ) in
      (`El_start ((xmlns,"insert"),attrs))::
      begin match hole.default with
        | None -> [ `El_end ]
        | Some rope -> Rope.(
          let patch = Patch.patch_hole ~partial:true in
          (* TODO: check the env ctxt *)
          let default = to_list ~patch env rope in
          match default with [] -> empty_seq | _ -> default
        ) @ [ `El_end ]
      end
  )

end

and Rope : XmlRope.S
  with type hole = Template.hole
   and type prov = Template.prov
   and type env  = Template.env
  = XmlRope.Make(Template)

let patch_hole = Patch.patch_hole

type t = Rope.t Scope.t

let default_rope = function lazy None -> Rope.empty | lazy (Some rope) -> rope

let get_attr loc tag attrs attr =
  try List.assoc ("",attr) attrs
  with Not_found -> raise (Error (`Missing_attribute (Some loc, tag, attr)))

let get_attr_opt attrs attr =
  try Some (List.assoc ("",attr) attrs)
  with Not_found -> None

module XmlStack = struct
  type 'a frame = int * (int * 'a) list
  type 'a t = 'a frame list

  let empty : 'a t = [1,[]]

  let consumep = function (0,_)::_ -> true | [] | _::_ -> false
  let push stack = (0,[])::stack
  let incr = function [] -> [] | (h,s)::t -> (h+1,s)::t
  let decr bindings = function
    | [] -> [], bindings
    | (h,(d,b)::bs)::t when h = d -> (h-1,bs)::t, b
    | (h,bs)::t -> (h-1,bs)::t, bindings
  let pop = function [] -> [] | _::t -> t
  let peek : _ -> Rope.t Closure.t = function
    | [] | (_,[])::_ -> Closure.empty
    | (_,(_,s)::_)::_ -> s

  let save_bindings bindings : 'a t -> 'a t = function
    | (d,[])::s -> (d,[d,bindings])::s
    | ((d,(ld,_)::_)::_) as s when ld = d -> s
    | (d,bs)::s -> (d,(d,bindings)::bs)::s
    | [] -> []

  let add_frame frame (stack : Rope.t Closure.t t) =
    match stack with
    | (d,[])::s -> (d,[d,Closure.(push empty frame)])::s
    | (d,(ld,root)::r)::s when ld = d ->
      (d,(ld,Closure.push root frame)::r)::s
    | (d,(((_,root)::_) as r))::s ->
      (d,(d,Closure.push root frame)::r)::s
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

let empty_blueprint () = Scope.empty ()

let xml_source xml_input =
  xml_input,
  let pos = Xmlm.pos xml_input in
  if Xmlm.eoi xml_input
  then None
  else Some (pos, Xmlm.input xml_input)

let bind ?(partial=false) ~sink out bindings rope =
  let patch = patch_hole ~partial in
  Rope.to_stream ~patch ~sink (Env.create bindings) out rope

let sink ?(partial=false) push =
  let depth = ref 0 in (* TODO: this isn't very nice... *)
  let start = ref true in
  let after_root = ref false in
  let rec output prov out = function
    | (`Data _ | `El_start _ | `El_end) as s when !start ->
      start := false;
      let out = push out (`Dtd None) in
      output prov out s
    | `Dtd _ as s when !start ->
      start := false;
      output prov out s
    | `Data s when !depth = 0 && is_ws s -> out
    | (`Data _ | `Dtd _) as signal ->
      if !after_root
      then raise (Error (`Data_after_root prov.Prov.loc));
      push out signal
    | (`El_start ((ns,tag),attrs)) when not partial && ns = xmlns ->
      (* TODO: this is wrong. fix it. *)
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
      push out signal
    | `El_end ->
      decr depth;
      if !depth = 0 then after_root := true;
      push out `El_end
  in
  fun prov out s -> List.fold_left (output prov) out s

let buffer_sink ?partial buffer =
  let ns_prefix = xmlns_map_default in
  let xml_out =
    Xmlm.make_output ~decl:false ~nl:true ~ns_prefix (`Buffer buffer)
  in
  let polyglot = ref (Polyglot.Stream.smart_doc_of_xml_output xml_out) in
  let push stream signal = Polyglot.Stream.push_signals stream [signal] in
  let sink = sink ?partial push in
  fun prov () s -> polyglot := sink prov !polyglot s

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
    let _out = bind ~partial:true ~sink out empty rope in
    Buffer.contents buffer
*)

type ctxt = {
  acc : Rope.t; (* accumulated template*)
  local: Rope.t Closure.t XmlStack.t; (* local closure *)
  env : Rope.t Env.t; (* current environment *)
  tail : Ident.relative_t;
  path : Ident.absolute_t;
  (* invariant: env base scope should be at path + tail *)
}

let root_ctxt = {
  acc = Rope.empty;
  local = XmlStack.empty;
  env = Env.create (empty_blueprint ());
  path = Ident.root;
  tail = Ident.here;
}

(* TODO: we really should work out a way to signal intent to
   distinguish closure-links from dangling mistakes. *)
let fail_on_dangle scope = function
  | Ok (scope,_) -> scope
  | Err (`Dangling_link dst_id) -> scope
  (*let dst = Ident.to_string dst_id in
    let path = Ident.to_string path in
    raise (Error (`Dangling_link (prov, dst, path)))*)

let add_link (path : Ident.absolute_t) scope from to_ loc prov =
  let path_any = Ident.any path in
  (*let prov = Prov.with_loc prov loc in*)
  match from, to_ with
  | None, None -> scope, path_any (* TODO: something? *)
  | Some src, None ->
    let src = Ident.of_string loc src in
    fail_on_dangle scope (Scope.link Scope.overlay scope src path_any),
    src
  | None, Some dst ->
    let dst = Ident.of_string loc dst in
    fail_on_dangle scope (Scope.link Scope.overlay scope path_any dst),
    path_any
  | Some src, Some dst ->
    let src = Ident.of_string loc src in
    let dst = Ident.of_string loc dst in
    (*log Debug.scope Ident.("link "^(to_string src)^" -> "^(to_string dst));*)
    (*log Debug.scope (Scope.to_string scope);*)
    fail_on_dangle scope (Scope.link Scope.overlay scope src dst),
    src

let of_stream ~prov ~source =
  let open Scope in
  let rec run stack seq ctxt =
    assert Ident.(
      compare (any ctxt.path) (any (resolve (path ctxt.env.Env.base) ctxt.tail))
      = 0
    );
    function
    | acc, None ->
      let scope = ctxt.env.Env.base in
      acc, with_rope scope Rope.(ctxt.acc ++ (of_list ~prov (List.rev seq)))
    | acc, Some (loc, el) -> match el with
      | `El_start ((ns,el),attrs) when ns=xmlns ->
        handle stack seq ctxt acc attrs loc el
      | `El_start el ->
        let ctxt = { ctxt with local = XmlStack.incr ctxt.local } in
        run (XmlStack.incr stack) ((`El_start el)::seq) ctxt (source acc)
      | `El_end when XmlStack.consumep stack ->
        begin match XmlStack.pop stack with
          | [] ->
            let rope = Rope.(ctxt.acc ++ (of_list ~prov (List.rev seq))) in
            let scope = ctxt.env.Env.base in
            acc, with_rope scope rope
          | stack -> run stack seq ctxt (source acc)
        end
      | `El_end ->
        let stack, c = XmlStack.decr ctxt stack in
        let local, _ = XmlStack.(decr Closure.empty ctxt.local) in
        let path = c.path in
        let tail = c.tail in
        let ctxt = { ctxt with env = c.env; local; path; tail; } in
        run stack (`El_end::seq) ctxt (source acc)
      | (`Dtd _ | `Data _) as signal ->
        run stack (signal::seq) ctxt (source acc)

  and handle stack seq ctxt acc attrs loc = Rope.(function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      let name = get_attr loc "insert" attrs "name" in
      let to_= get_attr_opt attrs "with" in
      let base = ctxt.env.Env.base in
      let with_ = match to_ with
        | Some to_ -> Some (Ident.of_string loc to_)
        | None -> None
      in
      (*log Debug.scope ("insert "^name^(match to_ with
        | None -> ""
        | Some s -> " with "^s
      )^" @ "^Ident.(to_string (any ctxt.path)));*)
      let name_id = Ident.of_string loc name in
      let id = Ident.(append ctxt.tail name_id) in
      let local, with_exists = match with_ with
        | None -> (XmlStack.peek ctxt.local), true
        | Some with_ ->
          let local = XmlStack.peek ctxt.local in
          let local = Closure.(push local (link name_id with_)) in
          local,
          match get base Ident.(append ctxt.tail with_) with
          | Some _ -> true
          | None -> false
      in
      let with_ = match with_ with
        | Some with_ ->
          let with_ = Ident.(resolve ctxt.path with_) in
          Some with_
        | None -> None
      in
      (*log Debug.scope Ident.("getting rope for "^
                                 (to_string (any id)));*)
      (*log Debug.scope (Scope.to_string base);*)
      begin match get_rope base id, with_exists with
        | Err (`Not_found | `No_template), _ | _, false ->
          (*log Debug.scope ("add hole for "^Ident.(to_string (any id)));*)
          (*log Debug.scope (Scope.to_string ctxt.env.Env.base);*)
          (*log Debug.scope ("local closure");*)
          (*log Debug.scope (Closure.to_string local);*)
          let stack, ctxt, c =
            add_hole stack ctxt local acc name_id ?with_ literal loc
          in
          run stack [] ctxt c
        | Ok template, true ->
          let prov = Prov.with_loc prov loc in
          let scope = Scope.get_obj base (Ident.any ctxt.tail) in
          let base = match with_ with
            | None -> scope
            | Some _ -> fst (add_link ctxt.path scope (Some name) to_ loc prov)
          in
          let env = { ctxt.env with Env.base } in
          let path = path base in
          let ctxtb = { ctxt with tail = Ident.here; env; path; } in
          match Env.expand prov ctxtb.env id with
          | Err `Empty_hole ->
            raise (Error (`Empty_hole (Some prov, Ident.to_string id)))
          | Err (`Disallowed_expansion x) ->
            raise (Error (`Disallowed_expansion x))
          | Ok env ->
            let template = Rope.map_prov (fun parent ->
              let ident_s = Ident.to_string id in
              Prov.append_incl parent ident_s prov
            ) template in
            let patch_hole = patch_hole ~partial:true in
            let template = patch patch_hole env template in
            (*log Debug.scope ("done compile patching "^Ident.to_string id);*)
            (* Now, we throw away any default value. *)
            let acc, _default = run [0,[]] [] ctxtb (source acc) in
            let rope = ctxt.acc ++ literal ++ template in
            run stack [] { ctxt with acc = rope } (source acc)
      end

    | "attr" ->
      let name = get_attr loc "attr" attrs "name" in
      let subctxt = { ctxt with acc = Rope.empty } in
      let acc, content = run [0,[]] [] subctxt (source acc) in
      let rope = match rtrim_all seq with
        | (`El_start (tag,attrs))::r ->
          let literal = of_list ~prov (List.rev r) in
          let attr = (("",name), default_rope content.rope) in
          ctxt.acc ++ literal ++ (make_attrs ~prov (tag,attrs) [attr])
        | _::_ -> raise (Error (`Floating_attr (loc, name)))
        | [] ->
          if in_attrs ctxt.acc
          then
            let attr = (("",name), default_rope content.rope) in
            with_attr ctxt.acc attr
          else raise (Error (`Floating_attr (loc, name)))
      in
      run stack [] { ctxt with acc = rope } (source acc)

    | "link" ->
      (* TODO: Check for dangliness? *)
      let seq = rtrim seq in
      let from = get_attr_opt attrs "from" in
      let to_ = get_attr_opt attrs "to" in
      let subctxt = { ctxt with acc = Rope.empty } in
      (* TODO: Warn that this is being thrown away (for now)? *)
      let acc, _content = run [0,[]] [] subctxt (source acc) in
      let base = ctxt.env.Env.base in
      let from_id = match from with
        | Some from -> Ident.of_string loc from
        | None -> Ident.any ctxt.path
      in
      let to_id = match to_ with
        | Some to_ -> Ident.of_string loc to_
        | None -> Ident.any ctxt.path
      in
      let local = XmlStack.add_frame (Closure.link from_id to_id) ctxt.local in
      let scope,_ = add_link ctxt.path base from to_ loc prov in
      let stack = XmlStack.save_bindings ctxt stack in
      (*log Debug.scope ("scope after explicit link");*)
      (*log Debug.scope (Scope.to_string scope);*)
      let base_path = Ident.any (path base) in
      let env = { ctxt.env with Env.base = get_obj scope base_path } in
      run stack seq { ctxt with env; local; } (source acc)

    | "seq" ->
      let ctxt = { ctxt with local = XmlStack.push ctxt.local } in
      run (XmlStack.push stack) seq ctxt (source acc)

    | "let" ->
      let seq = rtrim seq in
      let name = Ident.of_string loc (get_attr loc "let" attrs "name") in
      (*log Debug.scope Ident.("let "^(to_string name)
                             ^"@"^(to_string (any ctxt.path)));*)
      (*log Debug.scope (Scope.to_string ctxt.env.Env.base);*)
      let base = ctxt.env.Env.base in
      let stack = XmlStack.save_bindings ctxt stack in
      let path = Ident.resolve ctxt.path name in
      let subbase, tail = Scope.split base (Ident.append ctxt.tail name) in
      let subctxt = {
        acc = Rope.empty; local = XmlStack.empty;
        env = { ctxt.env with Env.base = subbase }; path; tail;
      } in
      let path = Ident.any path in
      let acc, letb = run [0,[]] [] subctxt (source acc) in
      let rope = match letb.rope with
        | lazy None -> letb.rope
        | lazy (Some r) ->
          (* TODO: This would be cheaper in-line with accumulation *)
          let newr = Rope.map_signals rtrim_all r in
          if Rope.is_empty newr
          then Lazy.from_val None
          else Lazy.from_val (Some (Rope.optimize r))
      in
      let letb = Scope.get_obj letb path in
      let letb = with_maybe_rope letb rope in
      (*log Debug.scope ("after recurse "^(Ident.to_string name));*)
      (*log Debug.scope (Scope.to_string letb);*)
      let scope = Scope.put base path letb in
      (*log Debug.scope ("after put "^(Ident.to_string name));*)
      (*log Debug.scope (Scope.to_string scope);*)
      let scope = Scope.get_obj scope (Ident.any ctxt.tail) in
      (*log Debug.scope ("after pos reset "^(Ident.to_string name));*)
      (*log Debug.scope (Scope.to_string scope);*)
      let env = { ctxt.env with Env.base = scope } in
      let local = XmlStack.add_frame (Closure.scope name letb) ctxt.local in
      let ctxt = { ctxt with local; env; tail = Ident.here } in
      run stack seq ctxt (source acc)

    | "if" ->
      let exists = get_attr_opt attrs "exists" in
      let insert_now () =
        let ctxt = { ctxt with local = XmlStack.push ctxt.local } in
        run (XmlStack.push stack) seq ctxt (source acc)
      in
      let rec check_for_else ctxt c = match c with
        | _, None -> c, None
        | acc, Some (loc, el) -> match el with
          | `El_start ((ns,"else"),attrs) when ns=xmlns ->
            (* TODO: check for empty attrs *)
            let acc, elseb = run [0,[]] [] ctxt (source acc) in
            source acc, Some elseb
          | `Data d when is_ws d -> check_for_else ctxt (source acc)
          | (`El_start _ | `El_end | `Dtd _ | `Data _) -> c, None
      in
      let defer_on cond =
        (* TODO: Test local closure capture! *)
        let subctxt = { ctxt with acc = Rope.empty } in
        let acc, body = run [0,[]] [] subctxt (source acc) in
        let body_rope = default_rope body.rope in
        let subctxt = { ctxt with acc = Rope.empty } in
        let c, else_body = check_for_else subctxt (source acc) in
        let els = match else_body with
          | Some e -> Some (default_rope e.rope)
          | None -> None
        in
        let hole = Hole.new_conditional cond body_rope ?els () in
        let literal = of_list ~prov (List.rev seq) in
        let rope = ctxt.acc ++ literal ++ (make_hole ~prov hole) in
        run stack [] { ctxt with acc = rope } c
      in
      (* TODO: insert now with else *)
      begin match exists with
        | None -> insert_now ()
        | Some id ->
          let exid = Ident.of_string loc id in
          match Scope.get ctxt.env.Env.base (Ident.append ctxt.tail exid) with
          | Some _ -> insert_now ()
          | None -> defer_on (Condition.Exists exid)
      end

    | "else" ->
      raise (Error (`Floating_else loc))

    | "snoop" ->
      let literal = of_list ~prov (List.rev seq) in
      let label = get_attr loc "snoop" attrs "label" in
      let on = get_attr_opt attrs "on" in
      let subctxt = { ctxt with acc = Rope.empty } in
      let acc, _content = run [0,[]] [] subctxt (source acc) in
      let hole = Hole.new_snoop loc label on in
      let rope = ctxt.acc ++ literal ++ (make_hole ~prov hole) in
      run stack [] { ctxt with acc = rope } (source acc)

    | el ->
      raise (Error (`Unknown_tag (loc, el)))
  )

  and add_hole stack ctxt local acc name ?with_ literal loc =
    let open Rope in
    let prov = Prov.with_loc prov loc in
    match source acc with
    | (_, (None | Some (_,`El_end))) as c ->
      let hole = Hole.new_named ?with_ name local in
      let acc = ctxt.acc ++ literal ++ (make_hole ~prov hole) in
      let ctxt = { ctxt with local = XmlStack.push ctxt.local; acc; } in
      (XmlStack.push stack, ctxt, c)
    | (acc, _signal_opt) as c ->
      let subctxt = { ctxt with acc = Rope.empty } in
      let acc, default = run [0,[]] [] subctxt c in
      let default_rope = default_rope default.rope in
      let hole = Hole.new_valued ?with_ name default_rope local in
      let rope = ctxt.acc ++ literal ++ (make_hole ~prov hole) in
      (stack, { ctxt with acc = rope }, source acc)
  in
  fun acc ->
    run XmlStack.empty [] root_ctxt (source acc)

module Tree = struct
  let empty = Scope.empty

  (* TODO: raise if ident is invalid *)
  let add key obj scope =
    Scope.put scope (Ident.of_string (-1,-1) key) obj

  let tag tag = (tag, empty ())

  let of_kv kv = List.fold_left (fun scope (key, obj) ->
    add key obj scope
  ) (empty ()) kv

  let of_kv_maybe kv = List.fold_left (fun scope -> function
    | key, None -> scope
    | key, Some obj -> add key obj scope
  ) (empty ()) kv

  let of_string s = { (empty ()) with Scope.rope = Lazy.from_fun (fun () ->
    let prov = Prov.({
      src = Stream "Blueprint.Tree.of_string";
      loc = None;
      incl = None;
    }) in
    Some (Rope.of_data ~prov s)
  ); }

  let of_kv_string kv =
    of_kv (List.map (fun (car,cdr) -> (car, of_string cdr)) kv)

  let of_list list =
    let list = List.rev list in
    match List.fold_left (fun scope next -> match scope with
      | Some scope -> Some (of_kv ["head", next; "tail", scope])
      | None -> Some (of_kv ["head", next])
    ) None list with
    | Some t -> t
    | None -> (empty ())

  let of_cons name blue = of_kv [ name, blue ]

  let of_cons_string name s = of_kv [ name, of_string s ]

  let of_lazy_tree fn =
    { (empty ()) with Scope.children = Lazy.from_fun (fun () ->
        Lazy.force (fn ()).Scope.children
      );
    }

  (* This is sad. Is there a better way? *)
  let rec root scope =
    let { Scope.id; children } = scope in
    let children = Lazy.from_fun (fun () ->
      StringMap.mapi (fun k v ->
        root { v with Scope.id = Ident.extend id k }
      ) (Lazy.force children)
    ) in
    { scope with Scope.children }
end
