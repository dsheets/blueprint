(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

type 'a deque = 'a list * 'a list

module type TEMPLATE = sig
  type hole
  type prov
  type env

  val signals_of_hole : prov:prov -> env -> hole -> Xmlm.signal list
end

module type S = sig
  type hole
  type prov
  type env
  type t
  type patch_action = Retain | Recurse of env * t | Replace of t
  type patch = env -> prov -> hole -> patch_action
  type 'out sink = prov -> 'out -> Xmlm.signal list -> 'out

  val of_data : prov:prov -> string -> t

  val of_list : prov:prov -> Xmlm.signal list -> t

  val make_hole : prov:prov -> hole -> t

  val make_attrs : prov:prov -> Xmlm.tag -> (Xmlm.name * t) list -> t
  val in_attrs : t -> bool
  val with_attr : t -> Xmlm.name * t -> t

  val concat : t -> t -> t
  val ( ++ ) : t -> t -> t
  val ( +? ) : t option -> t -> t

  val empty : t

  val is_empty : t -> bool

  val map_signals : (Xmlm.signal list -> Xmlm.signal list) -> t -> t

  val optimize : t -> t

  val of_tree :
    prov:prov ->
    ([< `Data of string | `El of Xmlm.tag * 'b list ] as 'b) list -> t

  val holes : t -> hole list

  val map_prov : (prov -> prov) -> t -> t

  val patch : patch -> env -> t -> t

  val to_stream : patch:patch -> sink:'out sink -> env ->
    'out -> t -> 'out

  val to_list : patch:patch -> env -> t -> Xmlm.signal list
end

module Make(M : TEMPLATE) : S
  with type hole = M.hole
   and type prov = M.prov
   and type env  = M.env =
struct
  type hole = M.hole
  type prov = M.prov
  type env  = M.env

  (* TODO: could include indexes for fast traversal/lookup *)
  type t =
    | Literal of M.prov * Xmlm.signal deque
    | Hole of M.prov * M.hole
    | Attrs of M.prov * Xmlm.tag * (Xmlm.name * t) list
    | Sequence of t deque

  type patch_action = Retain | Recurse of env * t | Replace of t
  type patch = env -> prov -> hole -> patch_action

  type 'out sink = prov -> 'out -> Xmlm.signal list -> 'out

  let of_data ~prov data = Literal (prov, ([`Data data],[]))

  let of_list ~prov list = Literal (prov, (list,[]))

  let make_hole ~prov hole = Hole (prov, hole)
  let make_attrs ~prov tag attrs = Attrs (prov, tag, attrs)
  let rec in_attrs = function
    | Literal (_,_) | Hole (_,_) | Sequence ([],[]) -> false
    | Attrs (_,_,_) -> true
    | Sequence (_,rope::_) -> in_attrs rope
    | Sequence (fropes,[]) -> in_attrs (Sequence ([],List.rev fropes))
  let rec with_attr rope attr = match rope with
    | Literal (_,_) | Hole (_,_) | Sequence ([],[]) -> rope
    | Attrs (prov, tag, attrs) -> Attrs (prov, tag, attr::attrs)
    | Sequence (fropes,rope::rropes) ->
      Sequence (fropes, (with_attr rope attr)::rropes)
    | Sequence (fropes,[]) -> with_attr (Sequence ([],List.rev fropes)) attr

  (* TODO: could optimize if prov = prov' *)
  let concat a b = match a, b with
    | Sequence (fropes, rropes), atom -> Sequence (fropes, atom::rropes)
    | atom, Sequence (fropes, rropes) -> Sequence (atom::fropes, rropes)
    | atom, atom' -> Sequence ([atom; atom'], [])

  let (++) = concat
  let (+?) lhs rhs = match lhs with None -> rhs | Some lhs -> lhs ++ rhs

  let empty = Sequence ([],[])

  let rec is_empty = function
    | Literal (_,([],[])) -> true
    | Sequence (f,r) -> List.for_all is_empty f && List.for_all is_empty r
    | Literal (_,_)
    | Attrs (_,_,_)
    | Hole (_,_) -> false

  (* TODO: Should re-arrange for optimal access *)
  let rec optimize = function
    | Literal (prov, (f, r)) -> Literal (prov, (f@(List.rev r), []))
    | Hole _ | Attrs _ as x -> x
    | Sequence ([x], []) -> optimize x
    | Sequence (xs, []) ->
      begin match opt_seq xs with
        | [] -> empty
        | [x] -> x
        | xs -> Sequence (xs, [])
      end
    | Sequence (f, r) -> Sequence (f@(List.rev r), [])
  and opt_seq = function
    | [] -> []
    | (Sequence (f,r))::xs -> (opt_seq (f@(List.rev r)))@(opt_seq xs)
    | x::xs -> (optimize x)::(opt_seq xs)

  (* TODO: Should optimize first in order to pass f contiguous signals *)
  let rec map_signals f x = match x with
    | Hole (_,_)
    | Attrs (_,_,_) -> x
    | Literal (prov,(forward,reverse)) ->
      Literal (prov,(f (forward @ (List.rev reverse)),[]))
    | Sequence (fropes,rropes) ->
      Sequence (List.map (map_signals f) (fropes @ (List.rev rropes)),[])

  let of_tree ~prov xml =
    let rec aux acc = function
      | `Data d -> (`Data d)::acc
      | `El (tag,contents) ->
        `El_end::(List.fold_left aux ((`El_start tag)::acc) contents)
    in
    Literal (prov, List.(rev (fold_left aux [] xml), []))

  let holes rope =
    let rec aux acc = function
      | Literal (_,_) -> acc
      | Hole (_, hole) -> hole::acc
      | Attrs (_, _, ropes) -> List.fold_left (fun acc (_name, rope) ->
        aux acc rope
      ) acc ropes
      | Sequence (fropes, rropes) ->
        List.(fold_left aux (fold_left aux acc fropes) (rev rropes))
    in
    List.rev (aux [] rope)

  let rec map_prov f = function
    | Literal (prov, ds) -> Literal (f prov, ds)
    | Sequence (fropes,rropes) ->
      Sequence (List.map (map_prov f) fropes, List.map (map_prov f) rropes)
    | Hole (prov, h) -> Hole (f prov, h) (* TODO: map into hole? *)
    | Attrs (prov, tag, ropes) ->
      Attrs (f prov, tag,
             List.map (fun (name,rope) -> (name, map_prov f rope)) ropes)

  (* TODO: should also optimize rope? *)
  let patch f acc rope =
    let rec aux acc = function
      | Literal (_,_) as r -> r
      | Sequence (fropes,rropes) ->
        Sequence (List.map (aux acc) fropes, List.map (aux acc) rropes)
      | Hole (prov, h) as hole ->
        begin match f acc prov h with
          | Recurse (acc, t) -> aux acc t
          | Replace t -> t
          | Retain -> hole
        end
      | Attrs (prov, tag, ropes) ->
        Attrs (prov, tag,
               List.map (fun (name,rope) -> (name, aux acc rope)) ropes)
    in
    aux acc rope

  (* A sink building a buffer without tags *)
  let rec drop_tag prov buf = function
    | [] -> buf
    | (`Dtd _ | `El_start (_,_) | `El_end)::r -> drop_tag prov buf r
    | (`Data s)::r -> Buffer.add_string buf s; drop_tag prov buf r

  let set_attr attrs attr value =
    let rec loop acc = function
      | [] -> List.rev ((attr,value)::acc)
      | (a,_)::r when a = attr -> List.rev_append acc ((attr,value)::r)
      | h::r -> loop (h::acc) r
    in
    loop [] attrs

  (* TODO: should use patch? *)
  let rec to_stream
    : 'out. patch:_ -> sink:'out sink -> 'a -> 'out -> _ -> 'out =
    fun ~patch ->
    let rec aux : 'out. _ -> _ -> 'out sink -> 'out -> _ -> 'out =
      fun acc patch sink out -> function
        | Literal (prov, (fs,rs)) ->
          sink prov (sink prov out fs) (List.rev rs)
        | Hole (prov, h) ->
          begin match patch acc prov h with
            | Recurse (acc, rope) -> aux acc patch sink out rope
            | Replace t -> aux acc (fun _ _ _ -> Retain) sink out t
            | Retain ->
              sink prov out (M.signals_of_hole ~prov acc h)
          end
        | Sequence (fropes, rropes) ->
          List.(fold_left (aux acc patch sink)
                  (fold_left (aux acc patch sink) out fropes)
                  (rev rropes))
        | Attrs (prov, (el,attrs), ropes) ->
          let attrs = List.fold_left (fun attrs (name,rope) ->
            let buf = Buffer.create 16 in
            let buf = aux acc patch drop_tag buf rope in
            set_attr attrs name (Buffer.contents buf)
          ) attrs (List.rev ropes) in
          sink prov out [`El_start (el,attrs)]
    in
    fun ~sink acc out rope ->
      aux acc patch sink out rope

  let to_list ~patch acc rope =
    let sink _ acc s = List.rev_append s acc in
    List.rev (to_stream ~patch ~sink acc [] rope)
end
