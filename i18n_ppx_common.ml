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

let mkmapper default ident apply =
  fun mapper expr ->
    match expr with
    | { pexp_desc = Pexp_extension ({ txt = "i18n"; loc }, pstr) } ->
      begin match pstr with
        | PStr [ { pstr_desc = Pstr_eval (e, a) } ] ->
          begin match e.pexp_desc with
            | Pexp_ident i ->
              ident expr e i
            | Pexp_apply ({ pexp_desc = Pexp_ident i } as e, args) ->
              let args = List.map (fun (l, e) -> l, mapper.expr mapper e) args in
              apply expr e i args
            | _ -> assert false end
        | _ -> assert false end
    | x -> default.expr mapper x
