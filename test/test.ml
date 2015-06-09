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

open OUnit

type result = Success | Error | Trouble
type return = {
  blue : result;
  i : result;
}

(* Utilities *)

let (/) = Filename.concat

let run command args ~stdin ~stdout ~stderr ?(env=[||]) () =
  Unix.create_process_env
    command (Array.append [|command|] args) env stdin stdout stderr

let after pid = Unix.(match waitpid [] pid with
  | (_,WEXITED k)   -> k
  | (_,WSIGNALED _) -> failwith "child unexpectedly signalled"
  | (_,WSTOPPED _)  -> failwith "child unexpectedly stopped"
)

let with_stdout f ~stdout =
  let stdin = Unix.(openfile "/dev/zero" [O_RDONLY] 0o000) in
  f ~stdin ~stdout ~stderr:Unix.stderr

let with_stderr f ~stderr =
  let stdin = Unix.(openfile "/dev/zero" [O_RDONLY] 0o000) in
  f ~stdin ~stdout:Unix.stderr ~stderr

let in_dir dir f =
  let pwd = Unix.getcwd () in
  try
    Unix.chdir dir;
    let r = f () in
    Unix.chdir pwd;
    r
  with e ->
    Unix.chdir pwd;
    raise e

let run_diff_test ~channel_fn ~dir ~result env =
  let stderr = Unix.stderr in
  let stdin, stdout = Unix.pipe () in
  let test_pid = in_dir dir (fun () ->
    channel_fn (run "test.sh" [||] ~env ()) stdout
  ) in
  Unix.close stdout;
  let diff_pid =
    run "diff" [|"-"; dir / result|] ~stdin ~stdout:stderr ~stderr ()
  in
  let test_exit = after test_pid in
  let diff_exit = after diff_pid in
  (test_exit, diff_exit)

let run_test ~expected_exit ~channel_fn ~dir ~result ~env ~name =
  let run_diff = run_diff_test ~channel_fn ~dir ~result in
  let (test_exit, diff_exit) = run_diff env in
  assert_equal ~msg:("exit"^name) ~printer:string_of_int expected_exit test_exit;
  assert_equal ~msg:("diff"^name) ~printer:string_of_int 0 diff_exit

let run_success_test dir name env () =
  let channel_fn f fd = with_stdout f ~stdout:fd in
  run_test
    ~expected_exit:0 ~channel_fn ~dir ~result:("out"^name^".xml") ~env ~name

let run_error_test dir name env () =
  let channel_fn f fd = with_stderr f ~stderr:fd in
  run_test
    ~expected_exit:1 ~channel_fn ~dir ~result:("err"^name^".txt") ~env ~name

let run_trouble_test dir name env () =
  let channel_fn f fd = with_stderr f ~stderr:fd in
  run_test
    ~expected_exit:2 ~channel_fn ~dir ~result:("trouble"^name^".txt") ~env ~name

let run_result = function
  | Success -> run_success_test
  | Error   -> run_error_test
  | Trouble -> run_trouble_test

let run_return_test return name () =
  run_result return.blue name ""   [|"OCAMLRUNPARAM=b";"BLUE=../../blue"|] ();
  run_result return.i    name ".i" [|"OCAMLRUNPARAM=b";"BLUE=../../blue -i"|] ()

(* Tests *)

let tests =
  List.map (fun (name,return) -> name, `Quick, run_return_test return name)

let success = { blue = Success; i = Success }
let error   = { blue = Error;   i = Error   }
let trouble = { blue = Trouble; i = Trouble }
let incomplete = { blue = Error; i = Success }

let success_tests ts = tests (List.map (fun name -> name, success) ts)

;;
Printexc.record_backtrace true;
Alcotest.run "blue" [

  "Simple", success_tests [
    "decl";
    "self_decl";
    "shadow_decl";
    "shadow_scope_decl";
    "shadow_scope_decl_multi";
    "rich_decl";
    "rich_decl_chained_open";
    "rich_decl_chained_closed";
    "top_seq_single";
    "top_seq_ws";
    "decl_local_let";
    "decl_double_open";
    "decl_hole_closed";
    "decl_dup";
  ];

  "Composition", success_tests [
    "top_seq_binder";
    "compose2";
    "compose3";
    "compose_stdin";
  ];

  "Default", success_tests [
    "empty_let";
    "default_insert";
    "empty_insert";
    "default_zero_insert";
    "override_default";
    "default_open";
  ];

  "Error", tests [
    "bad_xml_no_close", error;
    "self_hole", incomplete;
    "unknown_tag", error;
    "bad_decl_scope", incomplete;
    "bad_insert_name", error;
    "bad_let_name", error;
    "top_seq_multi", error;
    "decl_hole_open", incomplete;
    "decl_rec", incomplete;
    "decl_mutual_rec", incomplete;
    "decl_intro_rec", incomplete;
    "bad_compose_scope", incomplete;
    "compose3_rec", incomplete;
    "default_empty_insert", incomplete;
  ];

  "Trouble", tests [
    "enoent", trouble;
  ];
]
