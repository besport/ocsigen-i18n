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

(* Warning: Tsv file need to end with '\r' *)
{
type i18n_expr =
  | Var of string                    (* {{ user }} *)
  | Var_typed of string * string     (* {{ n %.2f }} *)
  | Str of string                    (* This is a string *)
  | Cond of string * string * string (* {{{ many ? s || }}} *)

let flush buffer acc =
  let acc = match String.escaped (Buffer.contents buffer) with
    | "" -> acc
    | x -> Str x :: acc in
  Buffer.clear buffer
; acc

}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let num = ['0'-'9']

let id = (lower | ['_']) (lower | upper | num | ['_'])*

rule parse_lines langs acc = parse
  | (id as key) '\t' {
      (* FIXME: Will break if List.map change its order of execution *)
      let tr = List.map (fun lang ->
          (lang, parse_expr (Buffer.create 0) [] lexbuf) ) langs in
    eol langs ((key, tr) :: acc) lexbuf }
  | eof { List.rev acc }

and eol langs acc = parse
  | [^'\n']* "\n" { Lexing.new_line lexbuf
                  ; parse_lines langs acc lexbuf}
  | eof { List.rev acc }

and parse_expr buffer acc = parse

  | "{{{" ' '* (id as c) ' '* "?" {
    let s1 = parse_string_1 (Buffer.create 0) lexbuf in
    let s2 = parse_string_2 (Buffer.create 0) lexbuf in
    let acc = flush buffer acc in
    parse_expr buffer (Cond (c, s1, s2) :: acc) lexbuf
  }

  | "{{" ' '* (id as x) ' '* "}}" {
      let acc = flush buffer acc in
      parse_expr buffer (Var x :: acc) lexbuf }

  | "{{" ' '* (id as x) ' '* ('%' [^ ' ' '}']+ as f)  ' '* "}}" {
      let acc = flush buffer acc in
      parse_expr buffer (Var_typed (x, f) :: acc) lexbuf }

  | '\t' | "" { List.rev (flush buffer acc ) }

  | [^ '\n' '\t'] as c { Buffer.add_char buffer c
                       ; parse_expr buffer acc lexbuf }

and parse_string_1 buffer = parse
  | "||" { String.escaped (Buffer.contents buffer) }
  | _ as c { Buffer.add_char buffer c
           ; parse_string_1 buffer lexbuf }

and parse_string_2 buffer = parse
  | "}}}" { String.escaped (Buffer.contents buffer) }
  | _ as c { Buffer.add_char buffer c
           ; parse_string_2 buffer lexbuf }

{

let print_list_of_languages_eliom fmt ~variants =
  Format.fprintf fmt
    "let%%shared languages = [%a]\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";")
       Format.pp_print_string) variants

let print_list_of_languages fmt ~variants =
  Format.fprintf fmt
    "let languages = [%a]\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";")
       Format.pp_print_string) variants

let print_type_eliom fmt ~variants =
  Format.fprintf fmt
    "[%%%%shared type t = %a]\n\
     [%%%%shared exception Unknown_language of string]\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "|")
       Format.pp_print_string) variants
let print_type fmt ~variants =
  Format.fprintf fmt
    "type t = %a\n\
     exception Unknown_language of string\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "|")
       Format.pp_print_string) variants
let print_header_eliom fmt ?primary_module ~default_language () =
  let server_language_reference =
    match primary_module with
    | None -> "Eliom_reference.Volatile.eref\n\
               ~scope:Eliom_common.default_process_scope default_language"
    | Some module_name -> module_name ^ "._language_"
  and client_language_reference =
    match primary_module with
    | None -> "ref default_language"
    | Some module_name -> module_name ^ "._language_"
  and default_lang =
    match primary_module with
    | None -> "let%shared default_language = " ^ default_language ^ "\n"
    | Some module_name -> ""
  in
  Format.pp_print_string fmt @@
  default_lang ^
   "let%server _language_ = " ^ server_language_reference ^ "\n\
   let%server get_language () = Eliom_reference.Volatile.get _language_\n\
   let%server set_language language = \n\
   Eliom_reference.Volatile.set _language_ language\n\
   \n\
   let%client _language_ = " ^ client_language_reference ^ "\n\
   let%client get_language () = !_language_\n\
   let%client set_language language = _language_ := language\n\
   \n\
   let%shared txt = Eliom_content.Html.F.txt\n\
"

let print_header fmt ?primary_module ~default_language () =
  let  default_lang =
    match primary_module with
    | None -> "let default_language = " ^ default_language ^ "\n"
    | Some module_name -> ""
  in
  Format.pp_print_string fmt @@
  default_lang ^
   "let _language_ = default_language \n\
   let get_language () = your_function_to_getting_language \n\
   let set_language language = \n\
   your_function_to_setting_language\n\
   \n\
   let txt = Eliom_content.Html.F.txt \n\
"

(** Print the function [string_of_language] returning the string representation of a
    value o type t. The string representation is simply the value as a string. For
    example, the string representation of [Us] is ["Us"]
*)
let print_string_of_language_eliom fmt ~variants ~strings =
  Format.pp_print_string fmt "let%shared string_of_language = function \n" ;
  List.iter2 (fun v s -> Format.fprintf fmt "| %s -> %S" v s)
    variants strings ;
  Format.pp_print_string fmt "\n"

let print_string_of_language fmt ~variants ~strings =
  Format.pp_print_string fmt "let string_of_language = function \n" ;
  List.iter2 (fun v s -> Format.fprintf fmt "| %s -> %S" v s)
    variants strings ;
  Format.pp_print_string fmt "\n"


(** Print the function [language_of_string] returning the value of type t which
    corresponds to the given string. The exception [Unknown_language] is raised with
    the given string if the language doesn't exist.
*)
let print_language_of_string_eliom fmt ~variants ~strings =
  Format.pp_print_string fmt "let%shared language_of_string = function\n" ;
  List.iter2 (fun v s -> Format.fprintf fmt "| %S -> %s" s v)
    variants strings ;
  Format.pp_print_string fmt "| s -> raise (Unknown_language s)\n"

let print_language_of_string fmt ~variants ~strings =
  Format.pp_print_string fmt "let language_of_string = function\n" ;
  List.iter2 (fun v s -> Format.fprintf fmt "| %S -> %s" s v)
    variants strings ;
  Format.pp_print_string fmt "| s -> raise (Unknown_language s)\n"

let print_guess_language_of_string_eliom fmt =
  Format.pp_print_string fmt
    "let%shared guess_language_of_string s = \n\
     try language_of_string s \n\
     with Unknown_language _ as e -> \n\
     try language_of_string (String.sub s 0 (String.index s '-')) \n\
     with Not_found -> \n\
     raise e \n"
let print_guess_language_of_string fmt =
  Format.pp_print_string fmt
    "let guess_language_of_string s = \n\
     try language_of_string s \n\
     with Unknown_language _ as e -> \n\
     try language_of_string (String.sub s 0 (String.index s '-')) \n\
     with Not_found -> \n\
     raise e \n"

type arg = M of string | O of string

let print_module_body_eliom print_expr =
  let args languages =
    let rec f a =
      function [] -> List.rev a
             | Var x :: t          -> f (M x :: a) t
             | Var_typed (x, _) :: t -> f (M x :: a) t
             | Cond (x, _, _) :: t -> f (O x :: a) t
             | _ :: t              -> f a t in
    List.map (f []) languages
    |> List.flatten
    |> List.sort_uniq compare in
  let print_args fmt args =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ')
      (fun fmt -> function
         | M x -> Format.fprintf fmt "~%s" x
         | O x -> Format.fprintf fmt "?(%s=false)" x) fmt args in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
    (fun fmt (key, tr) ->
       let args = args (List.map snd tr) in
       Format.fprintf fmt "let %s ?(lang = get_language ()) () %a () =\n\
                           match lang with\n%a"
         key
         print_args args
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
            (fun fmt (language, tr) ->
               Format.fprintf fmt "| %s -> %a"
                 language print_expr tr) ) tr )


let print_module_body primary_module print_expr =
  let args languages =
    let rec f a =
      function [] -> List.rev a
             | Var x :: t          -> f (M x :: a) t
             | Var_typed (x, _) :: t -> f (M x :: a) t
             | Cond (x, _, _) :: t -> f (O x :: a) t
             | _ :: t              -> f a t in
    List.map (f []) languages
    |> List.flatten
    |> List.sort_uniq compare in
  let print_args fmt args =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.pp_print_char fmt ' ')
      (fun fmt -> function
         | M x -> Format.fprintf fmt "~%s" x
         | O x -> Format.fprintf fmt "?(%s=false)" x) fmt args in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
    (fun fmt (key, tr) ->
       let args = args (List.map snd tr) in
       Format.fprintf fmt "let %s ?(lang = %s.get_language ()) () %a () =\n\
                           match lang with\n%a"
         key
         primary_module
         print_args args
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
            (fun fmt (language, tr) ->
               Format.fprintf fmt "| %s -> %a"
                 language print_expr tr) ) tr )

let pp_print_list fmt printer =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";")
       printer)

let print_expr_html fmt key_values =
  let print_key_value fmt =
    function
       | Str s -> Format.fprintf fmt "[txt \"%s\"]" s
       | Var v -> Format.pp_print_string fmt v
       | Var_typed (v, f) ->
         Format.fprintf fmt "[txt (Printf.sprintf \"%s\" %s)]" f v
       | Cond (c, s1, s2) ->
         Format.fprintf fmt "[txt (if %s then \"%s\" else \"%s\")]"
           c s1 s2
  in
  match key_values with
  | [] ->
     assert false
  | [key_value] ->
     print_key_value fmt key_value
  | _ ->
     Format.fprintf fmt "List.flatten " ;
     pp_print_list fmt print_key_value key_values

let print_expr_string fmt key_values =
  let print_key_value fmt =
    function
    | Str s -> Format.fprintf fmt "\"%s\"" s
    | Var v -> Format.pp_print_string fmt v
    | Var_typed (v, f) ->
       Format.fprintf fmt "(Printf.sprintf \"%s\" %s)" f v
    | Cond (c, s1, s2) ->
       Format.fprintf fmt "(if %s then \"%s\" else \"%s\")"
         c s1 s2
    in
    match key_values with
    | [] ->
       assert false
    | [key_value] ->
       print_key_value fmt key_value
    | _ ->
       Format.fprintf fmt "String.concat \"\" " ;
       pp_print_list fmt print_key_value key_values

let input_file = ref "-"
let output_file = ref "-"
let languages = ref ""
let default_language = ref ""
let external_type = ref false
let primary_file = ref ""
let file_part = ref ""
let options = Arg.align
    [ ( "--languages", Arg.Set_string languages
      , " Comma-separated languages (e.g. en,fr-fr, or Foo.Fr,Foo.Us if \
         using external types). \
         Must be ordered as in source TSV file.")
    ; ( "--default-language", Arg.Set_string default_language
      , " Set the default language (default is the first one in --languages).")
    ; ( "--input-file", Arg.Set_string input_file
      , " TSV file containing keys and translations. \
         If option is omited or set to -, read on stdin.")
    ; ( "--ouput-file", Arg.Set_string output_file
      , " File TSV file containing keys and translations. \
         If option is omited or set to -, write on stdout.")
    ; ( "--external-type", Arg.Set external_type
      , " Values passed to --languages option come from a predefined type \
         (do not generate the type nor from/to string functions).")
    ; ( "--primary", Arg.Set_string primary_file
      , " Generated file is secondary and depends on given primary file.")
    ; ( "--file-part", Arg.Set_string file_part
      , " Part of the file that's generated (header or body).")
    ]

let usage = "usage: ocsigen-i18n-generator [options] [< input] [> output]"

let _ = Arg.parse options (fun s -> ()) usage

let normalize_type ?primary_module s =
  let constr =
  String.lowercase_ascii s
  |> Str.(global_replace (regexp "-") "_")
  |> String.capitalize_ascii
  in
  match primary_module with
  | None -> constr
  | Some module_name -> module_name ^ "." ^ constr

let _ =
  let in_chan =
    match !input_file with
    | "-" -> stdin
    | file -> open_in file in
  let out_chan =
    match !output_file with
    | "-" -> stdout
    | file -> open_out file in
  let primary_module = match !primary_file with
  | "" -> None
  | file -> let base = Filename.remove_extension file in
            Some (String.capitalize_ascii base)
  in
  let strings = Str.split (Str.regexp ",") !languages in
  let variants =
    if !primary_file = "" || not (!external_type)
    then List.map (normalize_type ?primary_module) strings
    else strings in
  let default_language =
    match !default_language with
    | "" -> (List.hd variants)
    | x ->
      let x = normalize_type ?primary_module x in
      assert (List.mem x variants) ;
      x in
  let lexbuf = Lexing.from_channel in_chan in
  (try
     let key_values = parse_lines variants [] lexbuf in
     let output = Format.formatter_of_out_channel out_chan in
     if (!file_part = "header") then 
       ( print_type output ~variants
       ; print_string_of_language output ~variants ~strings
       ; print_language_of_string output ~variants ~strings
       ; print_guess_language_of_string output ;
     print_list_of_languages output ~variants ;
     print_header output ?primary_module ~default_language () ) else if (!file_part = "body") then (
     (* Format.pp_print_string output "[%%shared\n" ; *)
     Format.fprintf output "module Tr = struct\n" ;
     (match primary_module with
     | Some pm -> print_module_body pm print_expr_html output key_values 
     | None -> failwith "abnormal") ;
     print_string "error ? 2\n" ;
     Format.fprintf output "\nmodule S = struct\n" ;
     (match primary_module with
     | Some pm -> print_module_body pm print_expr_string output key_values
     | None -> failwith "abnormal") ;
     Format.fprintf output "\nend\n" ;
     Format.fprintf output "end\n" ;
     Format.pp_print_string output "]\n") else
     (
          if primary_module = None && not (!external_type) then
       ( print_type_eliom output ~variants
       ; print_string_of_language_eliom output ~variants ~strings
       ; print_language_of_string_eliom output ~variants ~strings
       ; print_guess_language_of_string_eliom output) ;
     print_list_of_languages_eliom output ~variants ;
     print_header output ?primary_module ~default_language () ;
     Format.pp_print_string output "[%%shared\n" ;
     Format.fprintf output "module Tr = struct\n" ;
     print_module_body_eliom print_expr_html output key_values ;
     Format.fprintf output "\nmodule S = struct\n" ;
     print_module_body_eliom print_expr_string output key_values ;
     Format.fprintf output "\nend\n" ;
     Format.fprintf output "end\n" ;
     Format.pp_print_string output "]\n"
     )
   with Failure msg ->
     failwith (Printf.sprintf "lined: %d"
                 lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum) ) ;
  close_in in_chan ;
  close_out out_chan
}
