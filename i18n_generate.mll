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
  | Var of string                    (* This is a string *)
  | Str of string                    (* {{ user }} *)
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

  | "{{" " "* (id as x) " "* "}}" {
      let acc = flush buffer acc in
      parse_expr buffer (Var x :: acc) lexbuf }

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

let print_list_of_langs fmt langs =
  let rec aux = function
  | [] -> "]"
  | [head] -> head ^ "]"
  | head :: tail -> head ^ ";" ^ (aux tail)
  in
  Format.pp_print_string fmt @@ "let%shared available_languages = [" ^ (aux langs) ^ "\n"

let print_type fmt langs =
  Format.fprintf fmt
    "[%%%%shared type t = %a]\n"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "|")
       Format.pp_print_string ) langs

let print_header fmt default_lang =
  Format.pp_print_string fmt @@
  "[%%shared let default_language = " ^ default_lang ^ "]\n\
   [%%shared exception Unknown_language of string]\n\
   [%%server\n\
   let _language_ =\n\
   Eliom_reference.Volatile.eref\n\
   ~scope:Eliom_common.default_process_scope default_language\n\
   let get_lang () = Eliom_reference.Volatile.get _language_\n\
   let set_lang lang = Eliom_reference.Volatile.set _language_ lang\n\
   ]\n\
   [%%client\n\
   let _language_ = ref default_language\n\
   let get_lang () = !_language_\n\
   let set_lang lang = _language_ := lang\n\
   ]\n\
   \n\
   [%%shared\n\
   [@@@ocaml.warning \"-27\"]\n\
   let pcdata = Eliom_content.Html.F.pcdata\n\
"

let lang_string fn pattern fmt langs =
  Format.fprintf fmt "let %s = function %a\n" fn
    (Format.pp_print_list (fun fmt x -> Format.fprintf fmt pattern x x) )
    langs

(** Print the function [string_of_lang] returning the string representation of a
    value o type t. The string representation is simply the value as a string. For
    example, the string representation of [Us] is ["Us"]
*)
let print_string_of_lang = lang_string "string_of_lang" "| %s -> %S"

(** Print the function [lang_of_string] returning the value of type t which
    corresponds to the given string. The exception [Unknown_language] is raised with
    the given string if the language doesn't exist.
*)
let print_lang_of_string fmt langs =
  lang_string "lang_of_string" "| %S -> %s" fmt langs ;
  Format.pp_print_string fmt "| s -> raise (Unknown_language s)\n"

let print_footer fmt = Format.pp_print_string fmt "]\n"

type arg = M of string | O of string

let print_module_body print_expr =
  let args langs =
    let rec f a =
      function [] -> List.rev a
             | Var x :: t          -> f (M x :: a) t
             | Cond (x, _, _) :: t -> f (O x :: a) t
             | _ :: t              -> f a t in
    List.map (f []) langs
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
       Format.fprintf fmt "let %s ?(lang = get_lang ()) () =\n\
                           match lang with\n%a"
         key
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.pp_print_string fmt "\n")
            (fun fmt (lang, tr) ->
               Format.fprintf fmt "| %s -> (fun %a () -> %a)"
                 lang print_args args print_expr tr) ) tr )

let pp_print_list fmt printer =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";")
       printer)

let print_expr_html fmt key_values =
  Format.fprintf fmt "List.flatten " ;
  pp_print_list fmt
    (fun fmt -> function
       | Str s -> Format.fprintf fmt "[pcdata \"%s\"]" s
       | Var v -> Format.pp_print_string fmt v
       | Cond (c, s1, s2) ->
         Format.fprintf fmt "[pcdata (if %s then \"%s\" else \"%s\")]"
           c s1 s2)
    key_values

let print_expr_string fmt key_values =
  Format.fprintf fmt "String.concat \"\" " ;
  pp_print_list fmt
    (fun fmt -> function
       | Str s -> Format.fprintf fmt "\"%s\"" s
       | Var v -> Format.pp_print_string fmt v
       | Cond (c, s1, s2) ->
         Format.fprintf fmt "(if %s then \"%s\" else \"%s\")"
           c s1 s2)
    key_values

let input_file = ref "-"
let output_file = ref "-"
let langs = ref ""
let default_lang = ref ""
let external_type = ref false

let options = Arg.align
    [ ( "--langs", Arg.Set_string langs
      , " Comma-separated langs (from ocaml sum type) (e.g. Us,Fr). \
         Must be ordered as in source TSV file.")
    ; ( "--default-lang", Arg.Set_string default_lang
      , " Set the default lang (default is the first one in --langs).")
    ; ( "--input-file", Arg.Set_string input_file
      , " TSV file containing keys and translations. \
         If option is omited or set to -, read on stdin.")
    ; ( "--ouput-file", Arg.Set_string output_file
      , " File TSV file containing keys and translations. \
         If option is omited or set to -, write on stdout.")
    ; ( "--external-type", Arg.Set external_type
      , " Values passed to --langs option come from a predefined type \
         (do not generate the type).")
    ]

let usage = "usage: ocsigen-i18n-generator [options] [< input] [> output]"

let _ = Arg.parse options (fun s -> ()) usage

let _ =
  let in_chan =
    match !input_file with
    | "-" -> stdin
    | file -> open_in file in
  let out_chan =
    match !output_file with
    | "-" -> stdout
    | file -> open_out file in
  let langs = Str.split (Str.regexp ",") !langs in
  let default_lang = match !default_lang with "" -> List.hd langs | x -> x in
  assert (List.mem default_lang langs) ;
  let lexbuf = Lexing.from_channel in_chan in
  (try let key_values = parse_lines langs [] lexbuf in
     let output = Format.formatter_of_out_channel out_chan in
     if not (!external_type) then print_type output langs ;
     print_list_of_langs output langs ;
     print_header output default_lang ;
     print_string_of_lang output langs ;
     print_lang_of_string output langs ;
     Format.fprintf output "module Tr = struct\n" ;
     print_module_body print_expr_html output key_values ;
     Format.fprintf output "\nmodule S = struct\n" ;
     print_module_body print_expr_string output key_values ;
     Format.fprintf output "\nend\n" ;
     Format.fprintf output "end\n" ;
     print_footer output
   with Failure msg ->
     failwith (Printf.sprintf "line: %d"
                 lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum) ) ;
  close_in in_chan ;
  close_out out_chan
}
