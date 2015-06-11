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

open Printf
open Cmdliner

let version = BlueprintVersion.(git_rev ^ (if git_dirty then " (dirty)" else ""))
let exec_name = Filename.basename Sys.argv.(0)

let ns_bind_default = function "t" -> Some Blueprint.xmlns | _ -> None
let ns = ns_bind_default

let ns_prefix = Blueprint.xmlns_map_default

let fatal_blueprint_error file err =
  eprintf "%s: %s: template error:\n%s\n%!"
    exec_name file (Blueprint.error_message err);
  exit 1

let read_blueprint file =
  try
    match file with
    | "-" ->
      let open Blueprint in
      let prov = File file in
      let xml_input = Xmlm.make_input ~ns (`Channel stdin) in
      let source = xml_source in
      let _, rope = of_stream ~prov ~source xml_input in
      rope
    | file ->
      Unix.handle_unix_error (Blueprint_unix.of_file ~ns) file
  with
  | Blueprint.Error err -> fatal_blueprint_error file err
  | Xmlm.Error ((line,col),err) ->
    eprintf "%s: %s: XML error at line %d column %d:\n%s\n%!"
      exec_name file line col (Xmlm.error_message err);
    exit 1

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

let rec compose prev_bindings incomplete = function
  | [] -> `Help (`Pager, None)
  | [file] ->
    let buffer = Buffer.create 1024 in
    let b = read_blueprint file in
    let xml_out =
      Xmlm.make_output ~decl:false ~nl:true ~ns_prefix (`Buffer buffer)
    in
    let depth = ref 0 in (* TODO: this isn't very nice... *)
    let after_root = ref false in
    let sink _prov out s = List.iter (function
      | `Data s when !depth = 0 && is_ws s -> ()
      | (`Data _ | `Dtd _) as signal ->
        if !after_root
        then fatal_blueprint_error file `Data_after_root;
        Xmlm.output out signal
      | (`El_start ((ns,tag),attrs))
        when not incomplete && ns = Blueprint.xmlns ->
        begin try let name = List.assoc ("","name") attrs in
            fatal_blueprint_error file (`Empty_hole name)
          with Not_found ->
            fatal_blueprint_error file (`Missing_attribute (tag, "name"))
        end
      | (`El_start _) as signal ->
        if !after_root
        then fatal_blueprint_error file `Element_after_root;
        incr depth;
        Xmlm.output out signal
      | `El_end ->
        decr depth;
        if !depth = 0 then after_root := true;
        Xmlm.output out `El_end
    ) s; out
    in begin
      try
        (* we get our xml_out back, ignore it *)
        ignore Blueprint.(
          bind ~incomplete ~sink xml_out prev_bindings (template b)
        )
      with
      | Blueprint.Error err -> fatal_blueprint_error file err
    end;
    Buffer.output_buffer stdout buffer;
    `Ok ()
  | file::files ->
    (* template is ignored in all but the last file *)
    let bindings = Blueprint.bindings (read_blueprint file) in
    compose (Blueprint.Bindings.append bindings prev_bindings) incomplete files

let compose_cmd =
  let doc = "templates to compose, use '-' to read from stdin" in
  let docv = "TEMPLATES" in
  let templates = Arg.(value (pos_all string [] & info ~docv ~doc [])) in
  let doc = "allow incomplete output" in
  let incomplete = Arg.(value (flag & info ~doc ["i"])) in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") composes blueprint templates.");
  ]
  in
  Term.(
    ret (pure (compose Blueprint.Bindings.empty) $ incomplete $ templates),
    info exec_name ~version ~man
  )

;;

match Term.eval compose_cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
