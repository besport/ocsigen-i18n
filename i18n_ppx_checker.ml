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

open Ast_mapper
open Asttypes
open Parsetree
open Longident

(* Compiler:
 * ocamlbuild -pkg compiler-libs.common -pkg str i18n_ppx_checker.native *)
(* Usage: i18n_ppx_checker.native files < i18n.tsv *)

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
  let ident expr e i =
    Hashtbl.replace keys (Longident.flatten i.txt |> List.rev |> List.hd) true
  ; expr in
  let apply expr e i args =
    Hashtbl.replace keys (Longident.flatten i.txt |> List.rev |> List.hd) true
  ; expr in
  let iterator =
    { default_mapper
      with expr = I18n_ppx_common.mkmapper default_mapper ident apply } in
  let process_file file =
    let ch = open_in file in
    let ast = Parse.implementation (Lexing.from_channel ch) in
    ignore (iterator.structure iterator ast) ;
    close_in ch in
  List.iter process_file files ;
  Hashtbl.iter (fun x b -> if not b then print_endline x) keys

let _ =
  check (List.tl (Array.to_list Sys.argv))
