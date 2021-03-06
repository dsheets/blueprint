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

let version = BlueprintVersion.(
  git_descr ^ (if git_dirty then " (dirty)" else "")
)
let exec_name = Filename.basename Sys.argv.(0)

let ns_bind_default = function "t" -> Some Blueprint.xmlns | _ -> None
let ns = ns_bind_default

let fatal_blueprint_error file err =
  eprintf "%s: %s: template error:\n%s\n%!"
    exec_name file (Blueprint.error_message err);
  exit 1

let read_blueprint file =
  try
    match file with
    | "-" ->
      let open Blueprint in
      let prov = Prov.({ src = File file; loc = None; incl = None }) in
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

let rec compose prev_bindings partial = function
  | [] -> `Help (`Pager, None)
  | [file] ->
    let b = read_blueprint file in
    let template = Blueprint.(default_rope (Scope.template b)) in
    Blueprint.(
      try
        bind_to_output ~partial stdout prev_bindings template;
        `Ok ()
      with
      | Error err -> fatal_blueprint_error file err
    )
  | file::files ->
    (* template is ignored in all but the last file *)
    let bindings = read_blueprint file in
    compose (Blueprint.Scope.overlay bindings prev_bindings) partial files

let compose_cmd =
  let doc = "templates to compose, use '-' to read from stdin" in
  let docv = "TEMPLATES" in
  let templates = Arg.(value (pos_all string [] & info ~docv ~doc [])) in
  let doc = "allow partially fulfilled output" in
  let partial = Arg.(value (flag & info ~doc ["p"])) in
  let man = [
    `S "DESCRIPTION";
    `P ("$(b, "^exec_name^") composes blueprint templates.");
  ]
  in
  Term.(
    ret (pure (compose (Blueprint.Scope.empty ())) $ partial $ templates),
    info exec_name ~version ~man
  )

;;

match Term.eval compose_cmd with
| `Ok () | `Version | `Help -> exit 0
| `Error _ -> exit 1
