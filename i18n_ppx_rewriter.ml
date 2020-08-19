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

let default_module_name = ref ""

let mk_ident module_name_o i =
  let module_name, safe_ident =
    match i.txt with 
    (* CASE 1: [%i18n ident ... ] *)
    | Lident _ -> !default_module_name, i.txt
    (* CASE 2: [%i18n S.ident ... ] *)
    | Ldot (Lident "S", _) -> !default_module_name, i.txt
    (* CASE 3: [%i18n OtherMod.ident ...] *)
    | Ldot (Lident module_name, s) -> module_name, Lident s
    (* CASE 4 : [%i18n OtherMod.S.ident ...] *)
    | Ldot (Ldot (Lident module_name, "S"), s) ->
      module_name, Ldot (Lident "S", s)
    (* CASE X : [%i18n OtherModA.OtherModB.ident ...]. Illegal. *)
    | i -> let err_msg =
             Format.asprintf "%a is not a valid i18n expression"
               Pprintast.longident i
      in failwith err_msg
  in
  parse
    (module_name
     ^ ".Tr." ^ (String.concat "." (Longident.flatten safe_ident)))

let unit loc = [%expr ()]

let lang_args = function
  | ((Labelled("?lang"), _) as lang) :: args
  | ((Labelled("~lang"), _) as lang) :: args -> ([ lang ], args)
  | x -> ([], x)

let apply e args =
  let (lang, args) = lang_args args in
  Pexp_apply (e, lang @ [ ( Nolabel, unit e.pexp_loc ) ]
                 @ args @ [ ( Nolabel, unit e.pexp_loc ) ] )

let ident module_name_o expr e i =
  let e' =
    { e with pexp_desc = Pexp_ident { i with txt = mk_ident module_name_o i } }
  in
  { expr with pexp_desc = apply e' [] }

let apply module_name_o expr e i args =
  let e' =
    { e with pexp_desc = Pexp_ident { i with txt = mk_ident module_name_o i } }
  in
  { expr with pexp_desc = apply e' args }

(* Usage: -ppx "i18n_ppx_rewrite.native Module_name" *)
let _ =
  register "i18n" (fun argv ->
      default_module_name := List.hd argv ;
      { default_mapper
        with expr = I18n_ppx_common.mkmapper default_mapper ident apply  })
