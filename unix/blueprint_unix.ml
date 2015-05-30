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

open Blueprint

let of_file ?(ns=fun _ -> None) path =
  let prov = File path in
  let fd = Unix.(openfile path [O_RDONLY] 0o000) in
  let ic = Unix.in_channel_of_descr fd in
  try
    let xml_input = Xmlm.make_input ~ns (`Channel ic) in
    let source = xml_source in
    let _, rope = of_stream ~prov ~source xml_input in
    close_in ic;
    rope
  with e -> (* TODO: Actually handle exceptions *)
    close_in ic;
    raise e
