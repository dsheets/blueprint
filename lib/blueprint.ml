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
  let v = 0 (*scope lor get*)
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
  | `Disallowed_traversal of Prov.t * string
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

let error_message : error -> string = Printf.(function
  | `Empty_hole (None, name) ->
    sprintf "No value for hole named '%s'" name
  | `Empty_hole (Some prov, name) ->
    sprintf "No value for hole named '%s'\n%s" name (Prov.to_string prov)
  | `Dangling_link (prov, expanding, name) ->
    sprintf "Dangling link to '%s' prevented expansion of '%s'\n%s"
      name expanding (Prov.to_string prov)
  | `Disallowed_expansion (prov, name) ->
    sprintf "Disallowed expansion of '%s'\n%s"
      name (Prov.to_string prov)
  | `Disallowed_traversal (prov, name) ->
    sprintf "Disallowed traversal of '%s'\n%s" name (Prov.to_string prov)
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

  let extend : any_t -> string -> any_t = fun ident segment ->
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
end

module Condition = struct
  type t =
    | Exists of Ident.any_t
end

module StringMap = Map.Make(String)

module IdentSet = Set.Make(Ident.Absolute)

module Scope = struct
  type link = {
    base : Ident.absolute_t; (* the context in which the link was created *)
    target : Ident.any_t;
    location : Prov.t;
  }
  type 'rope t = {
    rope : 'rope option Lazy.t;
    children : 'rope obj StringMap.t Lazy.t;
    parent : (string * 'rope path * 'rope t) option;
    expanded : bool;
  }
  and 'rope obj =
    | Scope of 'rope t
    | Link of link
    | Stack of 'rope obj list
  and 'rope path =
    | Pscope
    | Pstack of 'rope obj list * 'rope obj list (* zipped stack *)

  let empty =
    let rope = Lazy.from_val None in
    let children = Lazy.from_val StringMap.empty in
    let parent = None in
    let expanded = false in
    { rope; children; parent; expanded; }

  let template { rope } = rope
  let children { children } = children

  let with_rope obj rope =
    { obj with rope = Lazy.from_fun (fun () -> Some rope) }

  let up = function
    | { parent = None } as n -> n
    | { parent = Some (name, path, p) } as n ->
      let children = Lazy.from_fun (fun () ->
        let lazy children = p.children in
        let obj = match path with
          | Pscope -> Scope n
          | Pstack (top,bot) -> Stack (List.rev_append top ((Scope n)::bot))
        in
        StringMap.add name obj children
      ) in
      { p with children }

  let rec to_root = function
    | { parent = None } as n -> n
    | n -> to_root (up n)

  let fold_up f z scope =
    let rec aux acc = function
      | { parent = None } -> acc
      | { parent = Some (name,path,p) } as n ->
        aux (f acc n name path p) (up n)
    in
    aux z scope

  let path scope =
    let ident = fold_up (fun ident _ name _ _ -> name::ident) [] scope in
    Ident.({ kind = Absolute; ident; loc = None })

  let to_string scope =
    let path = path scope in
    let scope = to_root scope in
    let open Printf in
    let buf = Buffer.create 64 in
    let rec print_obj p indent name = function
      | Scope scope ->
        Buffer.add_string buf (sprintf "\n%s%s" indent name);
        print (Ident.extend p name) (indent^"  ") scope
      | Link { base; target } ->
        let base = Ident.(to_string (any base)) in
        let target = Ident.(to_string (any target)) in
        Buffer.add_string buf
          (sprintf "\n%s%s --> %s/%s" indent name base target)
      | Stack objs ->
        Buffer.add_string buf (sprintf "\n%s(" indent);
        List.iter (fun obj -> print_obj p indent name obj) objs;
        Buffer.add_string buf (sprintf "%s)" indent)
    and print p indent { rope; children = lazy children; expanded } =
      let cursor = if p = path then "*" else "" in
      let has_rope = match rope with
        | lazy (Some _) -> "+"
        | lazy None -> ""
      in
      Buffer.add_string buf
        (sprintf "%s%s:%s" has_rope cursor (if expanded then "X" else ""));
      StringMap.iter (print_obj p indent) children;
      if StringMap.cardinal children = 0 then Buffer.add_string buf "\n"
    in
    print Ident.root "  " scope;
    Buffer.contents buf

  let merge_lazy_option a b = match a with
    | lazy (Some _) -> a
    | lazy None -> b

  let merge_lazy_map c a b = Lazy.from_fun (fun () ->
    StringMap.merge (fun _name a b ->
      match a, b with
      | None, None -> None
      | Some _, None -> a
      | None, Some _ -> b
      | Some a, Some b -> c a b
    ) (Lazy.force a) (Lazy.force b)
  )

  (* for checking whether we're in a path traversal loop via links *)
  let traverse traversed scope ident =
    let path = Ident.(resolve (path scope) ident) in
    if IdentSet.mem path traversed
    then Err (`Disallowed_traversal ident)
    else
      let traversed = IdentSet.add path traversed in
      Ok (traversed, scope)

  (* The small step implementation for get; preserves zipper parent invariants *)
  let rec down ~traversed scope obj name =
    (*log Debug.get ("descending "^name^"@"^(Ident.to_string (path scope)));*)
    match obj with
    | Scope scope ->
      begin try
          let lazy children = scope.children in
          match StringMap.find name children with
          | Scope child ->
            (*log Debug.get "child scope FOUND";*)
            let child = { child with parent = Some (name, Pscope, scope) } in
            Some (scope, Scope child)
          | obj ->
            (*log Debug.get "other obj FOUND";*)
            Some (scope, obj)
        with Not_found ->
          (*log Debug.get "child NOT FOUND";*)
          None
      end
    | Link { base; target; location } ->
      let ident = Ident.(any (resolve base target)) in
      (*log Debug.get ("following link to "^(Ident.to_string ident));*)
      begin match traverse traversed scope ident with
        | Err (`Disallowed_traversal id) ->
          None
        (* TODO: need better way to distinguish loops vs out of
           context/partial application *)
        (* TODO: right prov? *)
        (*raise (Error (`Disallowed_traversal (location, Ident.to_string id)))*)
        | Ok (traversed, scope) -> match get ~traversed scope ident with
          | None -> None
          | Some (Scope s as obj) -> down ~traversed s obj name
          | Some ((Link _ | Stack _) as obj) ->
            down ~traversed scope obj name
      end
    | Stack stack ->
      let rec down_stack acc = function
        | [] ->
          (*log Debug.get "no more stack";*)
          None
        | (Stack s)::objs -> down_stack acc (s@objs)
        | (Link _ as obj)::objs ->
          begin match down ~traversed scope obj name with
            | None -> down_stack (obj::acc) objs
            | Some _ as r -> r
          end
        | (Scope obj_scope)::objs ->
          (*log Debug.get ("down stack "^(string_of_int (List.length acc)));*)
          let parent = match obj_scope.parent with
            | None -> None
            | Some (name, _, _) -> Some (name, Pstack (acc,objs), scope)
          in
          let obj_scope = { obj_scope with parent } in
          let obj = Scope obj_scope in
          match down ~traversed obj_scope obj name with
          | None ->
            (*log Debug.get ("can't find "^name);*)
            down_stack (obj::acc) objs
          | Some _ as r -> r
      in
      down_stack [] stack
  and get_
    : 'a. _ -> (_ -> 'a) -> (_ -> _ -> _ -> 'a) -> _ -> Ident.any_t -> 'a =
    fun traversed some missing scope ident ->
      let rec aux scope obj : Ident.relative_t -> _ =
        function
        | { Ident.ident = [] } -> some obj
        | { Ident.ident = name::rest } as ident ->
          match down ~traversed scope obj name with
          | Some (scope, obj) ->
            aux scope obj { ident with Ident.ident = rest }
          | None -> missing scope obj ident
      in
      (*log Debug.get ("getting "^(Ident.to_string ident)^"@"^(Ident.to_string (path scope)));*)
      Ident.(match ident with
        | { kind = Absolute } ->
          let scope = to_root scope in
          let r = aux scope (Scope scope) { ident with kind = Relative } in
          (*log Debug.get ("returning "^(Ident.to_string ident));*)
          r
        | { kind = Relative } as ident ->
          let r = aux scope (Scope scope) ident in
          (*log Debug.get ("returning "^(Ident.to_string ident));*)
          r
      )
  and get ?(traversed=IdentSet.empty) scope ident =
    get_ traversed (fun x -> Some x) (fun _ _ _ -> None) scope ident

  let find scope name =
    try
      get scope (Ident.of_string (-1,-1) name)
    with Error (`Bad_ident _) -> None

  let rec last_link_base_target = function
    | [] -> None, Ident.here
    | (Link { base; target; })::_ -> Some base, target
    | (Scope _)::objs -> last_link_base_target objs
    | (Stack s)::objs -> last_link_base_target (s@objs)

  let link_base_target scope base = match get scope (Ident.any base) with
    | None | Some (Stack []) | Some (Scope _) -> None, Ident.here
    | Some (Link { base; target; }) -> Some base, target
    | Some (Stack objs) -> last_link_base_target objs

  (* Links have special behavior when stacking: their targets are
     resolved against the *current* topmost target of their base. *)
  let rec stack scope top rest = match top with
    | Stack [] -> Stack rest
    | Stack (x::xs) -> stack scope x (xs@rest)
    | Link ({ base; target } as link) ->
      let base_of_base_opt = function None -> base | Some base -> base in
      let base, target = match link_base_target scope base with
        | base_opt, ({ Ident.kind = Ident.Relative } as last_target) ->
          base_of_base_opt base_opt, Ident.(any (append last_target target))
        | base_opt, ({ Ident.kind = Ident.Absolute } as last_target) ->
          base_of_base_opt base_opt, Ident.(any (resolve last_target target))
      in
      Stack (Link { link with base; target }::rest)
    | Scope a ->
      (* TODO: Should this search the stack to perform the link
         stacking even with interstitial links of the parent? *)
      match rest with
      | [] -> top
      | (Scope b)::bot -> Stack (Scope (overlay a b)::bot)
      | (Stack s)::bot -> stack scope top (s@bot)
      | (Link _)::_ -> Stack (top::rest)
  and overlay a' b' = {
    rope = merge_lazy_option a'.rope b'.rope;
    children = merge_lazy_map (fun a b -> match a,b with
      | Scope a, Scope b -> Some (Scope (overlay a b))
      | (Scope _ | Link _), (Scope _ | Link _) -> Some (stack b' a [b])
      | Stack [], _ -> Some b
      | Stack (x::xs), Stack ys -> Some (stack b' x (xs@ys))
      | Stack (x::xs), (Scope _ | Link _) -> Some (stack b' x (xs@[b]))
      | _, Stack bot -> Some (stack b' a bot)
    ) a'.children b'.children;
    parent = b'.parent;
    expanded = a'.expanded && b'.expanded; (* TODO: could diverge? *)
  }

  let rec synthesize scope = function
    | { Ident.ident = [] } -> scope
    | { Ident.ident = name::rest } as ident ->
      let ident = { ident with Ident.ident = rest } in
      synthesize { empty with parent = Some (name, Pscope, scope) } ident

  let rec get_obj ?(traversed=IdentSet.empty) scope ident =
    let rec missing_obj scope obj ident = match obj with
      | Scope scope -> synthesize scope ident
      | Stack (a::_) -> missing_obj scope a ident
      | Stack [] -> assert false (* TODO: blegh *)
      | Link { base; target; location } ->
        (* TODO: can diverge? *)
        let target = Ident.(any (resolve base target)) in
        let obj = get_obj ~traversed scope target in
        missing_obj scope obj ident
    in
    get_ traversed (fun x -> x) (fun scope obj ident ->
      Scope (missing_obj scope obj ident)
    ) scope ident

  let rec get_shallow_scope scope ident =
    let rec aux = function
      | Scope n | Stack ((Scope n)::_) -> n (* TODO: set parent? *)
      | Link { base; target; } ->
        let target = Ident.(any (resolve base target)) in
        (* TODO: can diverge? *)
        get_shallow_scope scope target
      | Stack (a::_) -> aux a
      | Stack [] -> assert false (* TODO: blegh *)
    in
    (* We'll be pointing at the unresolved object *)
    aux (get_obj scope ident)

  let return dst_scope src_scope =
    get_shallow_scope src_scope (path dst_scope)

  (* rebase finds the longest prefix of ident in scope that points to
     a scope (rather than a link or a stack) *)
  (* TODO: this could be made more efficient *)
  let rebase scope (ident : Ident.any_t) =
    let rec aux acc scope (nxt : Ident.relative_t) rest =
      match get scope (Ident.any nxt) with
      | None | Some (Stack _ | Link _) -> acc, scope
      | Some (Scope scope) ->
        match Ident.next rest with
        | None -> rest, scope
        | Some (n, r) -> aux rest scope n r
    in
    let scope = Ident.(match ident with
      | { kind = Absolute } -> to_root scope
      | { kind = Relative } -> scope
    ) in
    let ident = Ident.({ ident with kind = Relative }) in
    match Ident.next ident with
    | None -> ident, scope
    | Some (nxt, rest) -> aux ident scope nxt rest

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
        | Some (Scope n) -> up (apply n f { ident with Ident.ident = rest })
        | Some (Link { base; target; }) ->
          let target = Ident.(any (resolve base target)) in
          let target = get_shallow_scope scope target in
          return scope (apply target f { ident with Ident.ident = rest })
        | Some (Stack []) -> aux None
        | Some (Stack (obj::objs)) ->
          let scope = aux (Some obj) in
          let lazy children = scope.children in
          let child = StringMap.find name children in
          let children = StringMap.add name (Stack (child::objs)) children in
          { scope with children = lazy children }
        | None ->
          let r = match Ident.prev ident with
            | Some (parent, last) ->
              let parent_synth = get_shallow_scope scope (Ident.any parent) in
              apply parent_synth f last
            | None -> scope
          in
          List.fold_left (fun n _ -> up n) r rest
      in
      aux (match down IdentSet.empty scope (Scope scope) name with
        | Some (_,obj) -> Some obj
        | None -> None
      )

  (* put replaces the obj at ident in scope *)
  let put scope ident obj =
    let set name parent = match obj with
      | Scope scope ->
        let parent = Some (name, Pscope, parent) in
        Some (Scope { scope with parent })
      | Link _ | Stack _ -> Some obj
    in
    Ident.(match ident with
      | { kind = Absolute } ->
        let path = path scope in
        let scope = apply (to_root scope) set { ident with kind = Relative } in
        get_shallow_scope scope path
      | { kind = Relative } as ident -> apply scope set ident
    )

  (* link creates a link or stacked link at ident in scope *)
  let link scope ident base target location =
    let link = Link { base; target; location } in
    let set name scope =
      try
        let lazy children = scope.children in
        match StringMap.find name children with
        | (Scope _ | Link _) as obj -> Some (stack scope link [obj])
        | Stack objs -> Some (stack scope link objs)
      with Not_found -> Some link
    in
    Ident.(match ident with
      | { kind = Absolute } ->
        let scope = apply (to_root scope) set { ident with kind = Relative } in
        (*log Debug.scope "after link placement";*)
        (*log Debug.scope (string_of_scope scope);*)
        rebase scope (Ident.any base)
      | { kind = Relative } as ident ->
        let tail, scope = rebase scope (Ident.any base) in
        tail, apply scope set (Ident.append tail ident)
    )

  (* get_rope finds the topmost rope for an ident *)
  let rec get_rope ?(traversed=IdentSet.empty) scope ident =
    let rec aux traversed = function
      | None | Some (Scope { rope = lazy None }) -> Err `Empty_hole
      | Some (Scope { rope = lazy (Some rope) }) -> Ok rope
      | Some (Link { base; target }) ->
        let target = Ident.(any (resolve base target)) in
        begin match traverse traversed scope target with
          | Ok (traversed, scope) -> get_rope ~traversed scope target
          | Err (`Disallowed_traversal _) as err -> err
        end
      | Some (Stack []) -> Err `Empty_hole
      | Some (Stack (obj::objs)) -> match aux traversed (Some obj) with
        | Err `Empty_hole -> aux traversed (Some (Stack objs))
        | (Ok _ | Err (`Disallowed_traversal _)) as r -> r
    in
    aux traversed (get scope ident)

  (* expand marks all ancestor scopes of ident in scope as
     expanded. It returns a rebase pair of the original location. It
     can also fail to expand due to an empty hole or a dangling link. *)
  let rec expand prov scope (original : Ident.absolute_t) ident =
    let rec aux = function
      | Link { base; target; } ->
        let target = Ident.(any (resolve base target)) in
        expand prov scope original target
      | Stack ((Stack s)::bot) -> aux (Stack (s@bot))
      | Stack ((Link link as obj)::bot) ->
        begin match aux obj with
          | Err `Empty_hole ->
            let original = Ident.(to_string (any original)) in
            let target = Ident.(to_string (any link.target)) in
            Err (`Dangling_link (prov, original, target))
          | (Ok _ | Err _) as r -> r
        end
      | Stack ((Scope s)::bot) ->
        let name = List.(hd (rev ident.Ident.ident)) in
        let parent = up s in
        aux (Scope { s with parent = Some (name, Pstack ([],bot), parent) })
      | Stack [] -> Err `Empty_hole
      | Scope target ->
        if target.expanded
        then Err (`Disallowed_expansion (prov, Ident.to_string (path target)))
        else
          let rec aux = function
            | { expanded = true } | { parent = None } as n ->
              let scope = { n with expanded = true } in
              (*log Debug.expand Ident.("expand rebasing "^(to_string original));*)
              (*log Debug.expand (to_string scope);*)
              let p, scope = rebase scope (Ident.any original) in
              (*log Debug.expand (to_string scope);*)
              (*log Debug.expand Ident.("expand moving to "
                                     ^(to_string (any p))^"@"
                                     ^(to_string (path scope)));*)
              Ok (p, scope)
            | { parent = Some (name,_,_) } as n ->
              (*log Debug.expand Ident.("expanding up "^name);*)
              (*log Debug.expand (to_string n);*)
              aux (up { n with expanded = true })
          in
          aux target
    in
    (*log Debug.expand Ident.("entering expand for "^(to_string ident));*)
    (*log Debug.expand (to_string scope);*)
    match get scope (Ident.any ident) with
    | None -> Err `Empty_hole
    | Some obj -> aux obj

end

module Hole = struct
  type ('rope, 'value) reference = {
    name : Ident.any_t;
    closure : 'rope Scope.t; (* local scope only *)
    default : 'value option;
    with_ : Ident.absolute_t option; (* only for reserialization *)
  }
  type ('rope, 'value) t =
    | Reference of ('rope, 'value) reference
    | Conditional of Condition.t * 'value * 'value option
    | Snoop of string

  let name { name } = name

  let new_named ?with_ name closure =
    Reference { name; default = None; closure; with_; }
  let new_valued ?with_ name value closure =
    Reference { name; default = Some value; closure; with_; }

  let new_conditional cond body ?els () = Conditional (cond, body, els)

  let new_snoop label = Snoop label
end

let empty_seq = [ `El_start ((xmlns,"seq"),[]); `El_end ]

module Env = struct
  type 'rope t = {
    base : 'rope Scope.t;
    path : Ident.relative_t;
  }

  let create base path = { base; path; }
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
    let tail_name = Ident.append env.Env.path ident in
    match Scope.get env.Env.base tail_name with Some _ -> true | None -> false

  let rec patch_hole ~partial : Rope.patch =
    fun env prov -> function
      | Hole.Snoop label as hole ->
        let dump = Rope.of_list ~prov [
          `El_start (("","pre"),[]);
          `Data (label ^"\n"^ (Scope.to_string env.Env.base));
          `El_end;
        ] in
        let trailer = Rope.(if partial then make_hole ~prov hole else empty) in
        Rope.(Replace (dump ++ trailer))
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
        (*log Debug.scope (Scope.to_string hole.Hole.closure);*)
        let base_path = Scope.path base in
        let root = Scope.to_root base in
        let closure_path = Ident.resolve base_path env.Env.path in
        let closure = hole.Hole.closure in
        let tail, base =
          Scope.(rebase (overlay closure root) (Ident.any closure_path))
        in
        (*log Debug.scope Ident.("rebase backed off "
                             ^(to_string (any tail))
                             ^" from "^(to_string (any closure_path)));*)
        let name = Hole.name hole in
        (*log Debug.scope Ident.(
          "expanding "^(to_string name)^"@"^(to_string (any tail))^" with"
          );*)
        (*log Debug.scope (Scope.to_string base);*)
        let tail_name = Ident.append tail name in
        let ident = Ident.resolve base_path tail_name in
        let env, result =
          match Scope.get_rope base tail_name with
          | Err (`Disallowed_traversal ident) ->
            (* TODO: This should build prov for useful backtrace
               (probably using the link prov somehow) *)
            env,
            (Err (`Disallowed_traversal (prov, Ident.to_string ident)))
          | Err `Empty_hole ->
            env,
            (Err (`Empty_hole (Some prov, Ident.(to_string (any ident)))))
          | Ok rope ->
            match Scope.expand prov base ident tail_name with
            | Err (`Disallowed_expansion _ | `Dangling_link _) as result ->
              env, result
            | Err `Empty_hole ->
              env, (Err (`Empty_hole (Some prov, Ident.(to_string (any ident)))))
            | Ok (path, base) -> { Env.base; path; }, Ok rope
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
              let name = Ident.(any (resolve base_path tail_name)) in
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
    | Snoop label -> [`El_start ((xmlns,"snoop"),[("","label"),label]); `El_end]
    | Conditional (cond, body, els) ->
      let attrs = attrs_of_cond cond in
      let patch = Patch.patch_hole ~partial:true in
      (`El_start ((xmlns,"if"),attrs))::begin
        (* TODO: check the env ctxt *)
        Rope.to_list ~patch env body
      end @ [ `El_end ] @ (match els with None -> [] | Some e ->
        (`El_start ((xmlns,"else"),[]))::(Rope.to_list ~patch env e)@[`El_end])
    | Reference hole ->
      let base_path = Scope.path env.Env.base in
      let ident = Ident.(resolve (resolve base_path env.Env.path) hole.name) in
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
  let peek = function
    | [] | (_,[])::_ -> Scope.empty
    | (_,(_,s)::_)::_ -> s

  let save_bindings bindings : 'a t -> 'a t = function
    | (d,[])::s -> (d,[d,bindings])::s
    | ((d,(ld,_)::_)::_) as s when ld = d -> s
    | (d,bs)::s -> (d,(d,bindings)::bs)::s
    | [] -> []

  let add_binding (name : Ident.absolute_t) value (stack : Rope.t Scope.t t) =
    let name = Ident.any name in
    match stack with
    | (d,[])::s -> (d,[d,Scope.put Scope.empty name value])::s
    | (d,(ld,root)::r)::s when ld = d ->
      (d,(ld,Scope.put root name value)::r)::s
    | (d,(((_,root)::_) as r))::s ->
      (d,(d,Scope.put root name value)::r)::s
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

let empty_blueprint = Scope.empty

let xml_source xml_input =
  xml_input,
  let pos = Xmlm.pos xml_input in
  if Xmlm.eoi xml_input
  then None
  else Some (pos, Xmlm.input xml_input)

let bind ?(partial=false) ~sink out bindings rope =
  let patch = patch_hole ~partial in
  Rope.to_stream ~patch ~sink (Env.create bindings Ident.here) out rope

let xml_sink ?(partial=false) () =
  let depth = ref 0 in (* TODO: this isn't very nice... *)
  let start = ref true in
  let after_root = ref false in
  let rec output prov out = function
    | (`Data _ | `El_start _ | `El_end) as s when !start ->
      start := false;
      Xmlm.output out (`Dtd None);
      output prov out s
    | `Dtd _ as s when !start ->
      start := false;
      output prov out s
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
  in
  fun prov out s -> List.iter (output prov out) s; out

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
    let _out = bind ~partial:true ~sink out empty rope in
    Buffer.contents buffer
*)

type ctxt = {
  acc : Rope.t; (* accumulated template*)
  path : Ident.absolute_t; (* actual path *)
  local: Rope.t Scope.t XmlStack.t; (* local closure *)
  scope: Rope.t Scope.t; (* current environment *)
  tail : Ident.relative_t; (* offset in current environment to actual node *)
  (* Should hold: scope + tail = path *)
}

let root_ctxt = {
  acc = Rope.empty;
  path = Ident.root;
  local = XmlStack.empty;
  scope = empty_blueprint;
  tail = Ident.here;
}

let add_link (path : Ident.absolute_t) tail scope from to_ prov loc =
  let path_any = Ident.any path in
  let prov = Prov.with_loc prov loc in
  match from, to_ with
  | None, None -> (tail, scope), path_any (* TODO: something? *)
  | Some src, None ->
    let src = Ident.(append tail (of_string loc src)) in
    Scope.link scope src path path_any prov, src
  | None, Some dst ->
    let dst = Ident.(append tail (of_string loc dst)) in
    Scope.link scope path_any path dst prov, path_any
  | Some src, Some dst ->
    let src = Ident.(append tail (of_string loc src)) in
    let dst = Ident.(append tail (of_string loc dst)) in
    (*log Debug.scope Ident.("link "^(to_string src)^" -> "^(to_string dst));*)
    (*log Debug.scope (Scope.to_string scope);*)
    Scope.link scope src path dst prov, src

let of_stream ~prov ~source =
  let open Scope in
  let rec run stack seq ctxt = function
    | acc, None ->
      acc, with_rope ctxt.scope Rope.(ctxt.acc ++ (of_list ~prov (List.rev seq)))
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
            acc, with_rope ctxt.scope rope
          | stack -> run stack seq ctxt (source acc)
        end
      | `El_end ->
        let stack, scope = XmlStack.decr ctxt.scope stack in
        let local, _ = XmlStack.(decr Scope.empty ctxt.local) in
        let ctxt = { ctxt with local } in
        run stack (`El_end::seq) { ctxt with scope } (source acc)
      | (`Dtd _ | `Data _) as signal ->
        run stack (signal::seq) ctxt (source acc)

  and handle stack seq ctxt acc attrs loc = Rope.(function
    | "insert" ->
      let literal = of_list ~prov (List.rev seq) in
      let name = get_attr loc "insert" attrs "name" in
      let to_= get_attr_opt attrs "with" in
      let path = ctxt.path in
      let with_ = match to_ with
        | Some to_ -> Some Ident.(resolve path (of_string loc to_))
        | None -> None
      in
      (*log Debug.scope ("insert "^name^(match to_ with
        | None -> ""
        | Some s -> " with "^s
      ));*)
      let ctxtb, local, with_exists = match with_ with
        | None -> ctxt, (XmlStack.peek ctxt.local), true
        | Some with_ ->
          let local = XmlStack.peek ctxt.local in
          let (_,local),_ =
            add_link path Ident.here local (Some name) to_ prov loc
          in
          let (t,s),_ =
            add_link path ctxt.tail ctxt.scope (Some name) to_ prov loc
          in
          { ctxt with scope = s; tail = t },
          Scope.to_root local,
          match get s Ident.(any (append t with_)) with
          | Some _ -> true
          | None -> false
      in
      let name = Ident.of_string loc name in
      let ident = Ident.resolve path name in
      (*log Debug.scope Ident.("getting rope for "^(to_string (any ident)));*)
      begin match get_rope ctxt.scope (Ident.any ident),
                  with_exists with
        | Err (`Disallowed_traversal err), _ ->
          raise (Error (`Disallowed_traversal (prov, Ident.to_string err)))
        | Err `Empty_hole, _ | _, false ->
          (*log Debug.scope ("add hole for "^Ident.(to_string (any ident)));*)
          (*log Debug.scope (Scope.to_string ctxt.scope);*)
          (*log Debug.scope ("local closure");*)
          (*log Debug.scope (Scope.to_string local);*)
          let stack, ctxth, c =
            add_hole stack ctxtb local acc name ?with_ literal loc
          in
          let ctxt = { ctxth with scope = ctxt.scope; tail = ctxt.tail; } in
          run stack [] ctxt c
        | Ok template, true ->
          let prov = Prov.with_loc prov loc in
          let id = Ident.any ident in
          match Scope.expand prov ctxtb.scope ident id with
          | Err `Empty_hole ->
            raise (Error (`Empty_hole (Some prov, Ident.to_string id)))
          | Err (`Disallowed_expansion x) ->
            raise (Error (`Disallowed_expansion x))
          | Err (`Dangling_link x) ->
            let stack, ctxth, c =
              add_hole stack ctxtb local acc name ?with_ literal loc
            in
            let ctxt = { ctxth with scope = ctxt.scope; tail = ctxt.tail; } in
            run stack [] ctxt c
          | Ok (tail, base) ->
            let template = Rope.map_prov (fun parent ->
              let ident_s = Ident.to_string id in
              Prov.append_incl parent ident_s prov
            ) template in
            let patch_hole = patch_hole ~partial:true in
            let template = patch patch_hole (Env.create base tail) template in
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
      let local = XmlStack.peek ctxt.local in
      let (ltl, local), lpath =
        add_link ctxt.path Ident.here local from to_ prov loc
      in
      let ltl = Ident.append ltl lpath in
      (*log Debug.scope ("link local for "^Ident.(to_string ltl));*)
      (*log Debug.scope (Scope.to_string local);*)
      let link_id = Ident.resolve (path local) ltl in
      let link = get_obj local ltl in
      let local = XmlStack.add_binding link_id link ctxt.local in
      (*log Debug.scope (Scope.to_string (XmlStack.peek local));*)
      let (tail, scope),_ =
        add_link ctxt.path ctxt.tail ctxt.scope from to_ prov loc
      in
      let stack = XmlStack.save_bindings ctxt.scope stack in
      run stack seq { ctxt with scope; tail; local; } (source acc)

    | "seq" ->
      let ctxt = { ctxt with local = XmlStack.push ctxt.local } in
      run (XmlStack.push stack) seq ctxt (source acc)

    | "let" ->
      let seq = rtrim seq in
      let name = Ident.of_string loc (get_attr loc "let" attrs "name") in
      (*log Debug.scope Ident.("let "^(to_string name)
                             ^"@"^(to_string (any ctxt.path))
                             ^"/"^(to_string (any ctxt.tail)));*)
      (*log Debug.scope (Scope.to_string ctxt.scope);*)
      let ident = Ident.(resolve ctxt.path (append ctxt.tail name)) in
      let stack = XmlStack.save_bindings ctxt.scope stack in
      let letb = Scope.get_shallow_scope ctxt.scope (Ident.any ident) in
      (*log Debug.scope Ident.("letb for "^(to_string name));*)
      (*log Debug.scope (Scope.to_string letb);*)
      let subctxt = {
        acc = Rope.empty; path = ident; local = XmlStack.empty;
        scope = letb; tail = Ident.here;
      } in
      let acc, letb = run [0,[]] [] subctxt (source acc) in
      let rope = match letb.rope with
        | lazy None -> letb.rope
        | lazy (Some r) ->
          (* TODO: This would be cheaper in-line with accumulation *)
          let newr = Rope.map_signals rtrim_all r in
          if Rope.is_empty newr
          then Lazy.from_val None
          else Lazy.from_val (Some r)
      in
      let letb = { letb with rope } in
      (*log Debug.scope ("after recurse "^(Ident.to_string name));*)
      (*log Debug.scope (Scope.to_string letb);*)
      let scope = Scope.put ctxt.scope (Ident.any ident) (Scope letb) in
      let ctxt = {
        ctxt with local = XmlStack.add_binding ident (Scope letb) ctxt.local;
                  scope;
      } in
      (*log Debug.scope ("after put "^(Ident.to_string name));*)
      (*log Debug.scope (Scope.to_string scope);*)
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
      begin match exists with
        | None -> insert_now ()
        | Some id ->
          let exid = Ident.of_string loc id in
          match Scope.get ctxt.scope exid with
          | Some _ -> insert_now ()
          | None -> defer_on (Condition.Exists exid)
      end

    | "else" ->
      raise (Error (`Floating_else loc))

    | "snoop" ->
      let literal = of_list ~prov (List.rev seq) in
      let label = get_attr loc "snoop" attrs "label" in
      let subctxt = { ctxt with acc = Rope.empty } in
      let acc, _content = run [0,[]] [] subctxt (source acc) in
      let hole = Hole.new_snoop label in
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
  let of_kv kv = List.fold_left (fun scope (key, obj) ->
    (* TODO: raise if ident is invalid *)
    Scope.put scope (Ident.of_string (-1,-1) key) (Scope.Scope obj)
  ) Scope.empty kv

  let of_string s = Scope.({ empty with rope = Lazy.from_fun (fun () ->
    let prov = Prov.({
      src = Stream "Blueprint.Tree.of_string";
      loc = None;
      incl = None;
    }) in
    Some (Rope.of_data ~prov s)
  ); })

  let of_kv_string kv =
    of_kv (List.map (fun (car,cdr) -> (car, of_string cdr)) kv)

  let of_list list =
    let list = List.rev list in
    match List.fold_left (fun scope next -> match scope with
      | Some scope -> Some (of_kv ["head", next; "tail", scope])
      | None -> Some (of_kv ["head", next])
    ) None list with
    | Some t -> t
    | None -> Scope.empty
end
