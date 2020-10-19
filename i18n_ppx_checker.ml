(*
 * Copyright (C) 2015 BeSport, Julien Sagot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Ppxlib

(* Compiler:
 * ocamlbuild -pkg compiler-libs.common -pkg str i18n_ppx_checker.native *)
(* Usage: i18n_ppx_checker.native files < i18n.tsv *)

let expand ident ~loc:location ~path:_ expr =
  Ppxlib.Ast_pattern.
  (parse (pexp_ident __ |> map1 ~f:(ident expr)
          ||| (pexp_apply (pexp_ident __) (many __)
               |> map2 ~f:(fun id _ -> ident expr id))))
    location expr (fun x -> x)

let check files =
  let k =
    let tab_re = Str.regexp "\t" in
    let rec read acc = match input_line stdin with
      | line -> begin match Str.split_delim tab_re line with
        | a :: _ -> read (String.escaped a :: acc)
        | _ -> assert false end
      | exception End_of_file -> acc
    in read [] in
  let keys = Hashtbl.create (List.length files) in
  List.iter (fun x -> Hashtbl.add keys x false) k ;
  let ident expr i =
    let name i =
      match i with
      | Ldot (_, nm) -> nm
      | Lident nm -> nm
      | Lapply _ -> assert false
    in
    Hashtbl.replace keys (name i) true;
    expr in
  let expand = expand ident in
  let extension =
    Ppxlib.Extension.declare
      "i18n"
      Ppxlib.Extension.Context.expression
      Ppxlib.Ast_pattern.(single_expr_payload __)
      expand
  in
  let rule = Ppxlib.Context_free.Rule.extension extension in
  let iterator = new Ppxlib.Context_free.map_top_down [rule] in
  let process_file file =
    let ch = open_in file in
    let ast = Ppxlib.Parse.implementation (Lexing.from_channel ch) in
    ignore (iterator#structure
              (Expansion_context.Base.top_level
                 ~tool_name:"i18n_ppx_checker" ~file_path:file)
              ast) ;
    close_in ch in
  List.iter process_file files ;
  Hashtbl.iter (fun x b -> if not b then print_endline x) keys

let _ =
  check (List.tl (Array.to_list Sys.argv))
