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

open! Ppxlib

let default_module_name = ref ""
let module_prefix = ref ""
let module_suffix = ref ""

let mk_ident i =
  if !default_module_name = "" then invalid_arg "Missing default module name";
  let module_name, safe_ident =
    match i with
    (* CASE 1: [%i18n ident ... ] *)
    | Lident _ -> !default_module_name, i
    (* CASE 2: [%i18n S.ident ... ] *)
    | Ldot (Lident "S", _) -> !default_module_name, i
    (* CASE 3: [%i18n OtherMod.ident ...] *)
    | Ldot (Lident module_name, s) ->
     !module_prefix ^ module_name ^ !module_suffix, Lident s
    (* CASE 4 : [%i18n OtherMod.S.ident ...] *)
    | Ldot (Ldot (Lident module_name, "S"), s) ->
     !module_prefix ^ module_name ^ !module_suffix, Ldot (Lident "S", s)
    (* CASE X : [%i18n OtherModA.OtherModB.ident ...]. Illegal. *)
    | i -> let err_msg =
             Format.asprintf "%s is not a valid i18n expression"
               (Longident.name i)
      in failwith err_msg
  in
  let rec prefix i =
    match i with
    | Lident id ->
       Ldot (Ldot (Lident module_name, "Tr"), id)
    | Ldot (i, nm) ->
       Ldot (prefix i, nm)
    | Lapply _ ->
       let err_msg =
             Format.asprintf "%s is not a valid i18n expression"
               (Longident.name i)
      in failwith err_msg
  in
  prefix safe_ident

let unit loc =
  let open Ast_builder.Default in
  (Nolabel, pexp_construct ~loc (Located.mk ~loc (Lident "()")) None)

let lang_args = function
  | ((Labelled("?lang"), _) as lang) :: args
  | ((Labelled("~lang"), _) as lang) :: args -> ([ lang ], args)
  | x -> ([], x)

let ident loc id =
  let open Ast_builder.Default in
  pexp_ident ~loc (Located.mk ~loc (mk_ident id))

let apply loc id args =
  let (lang, args) = lang_args args in
  let open Ast_builder.Default in
  pexp_apply ~loc (ident loc id) (lang @ [unit loc] @ args @ [unit loc])

let () =
  List.iter
    (fun (key, spec, doc) -> Ppxlib.Driver.add_arg key spec ~doc)
    ["--prefix", Arg.Set_string module_prefix,
     "PREFIX The prefix added to module names"
    ;"--suffix", Arg.Set_string module_suffix,
     "SUFFIX The suffix added to module names"
    ;"--default-module", Arg.Set_string default_module_name,
     "MODULE Name of the default module"]

let expand ~loc:loc0 ~path:_ expr =
  Ppxlib.Ast_pattern.
  (parse (pexp_ident __ |> map1 ~f:(fun id -> apply loc0 id [])
          ||| (pexp_apply (pexp_ident __) (many __)  |> map2 ~f:(apply loc0))))
    loc0 expr (fun x -> x)

let extension =
  Ppxlib.Extension.declare
    "i18n"
    Ppxlib.Extension.Context.expression
    Ppxlib.Ast_pattern.(single_expr_payload __)
    expand

let rule = Ppxlib.Context_free.Rule.extension extension

let () =
  Ppxlib.Driver.register_transformation ~rules:[rule] "i18n"
