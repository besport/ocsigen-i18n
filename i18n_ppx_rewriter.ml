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

let module_name = ref ""

let mk_ident i =
  parse (!module_name ^ ".Tr." ^ (String.concat "." (Longident.flatten i.txt)))

let unit loc = [%expr ()]

let lang_args = function
  | ((Labelled("?lang"), _) as lang) :: args
  | ((Labelled("~lang"), _) as lang) :: args -> ([ lang ], args)
  | x -> ([], x)

let apply e args =
  let (lang, args) = lang_args args in
  Pexp_apply (e, lang @ [ ( Nolabel, unit e.pexp_loc ) ]
                 @ args @ [ ( Nolabel, unit e.pexp_loc ) ] )

let ident expr e i =
  let e' = { e with pexp_desc = Pexp_ident { i with txt = mk_ident i } } in
  { expr with pexp_desc = apply e' [] }

let apply expr e i args =
  let e' = { e with pexp_desc = Pexp_ident { i with txt = mk_ident i } } in
  { expr with pexp_desc = apply e' args }

(* Usage: -ppx "i18n_ppx_rewrite.native Module_name" *)
let _ =
  register "i18n" (fun argv ->
    module_name := List.hd argv ;
    { default_mapper
      with expr = I18n_ppx_common.mkmapper default_mapper ident apply  })
