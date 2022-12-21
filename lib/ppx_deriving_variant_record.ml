module List = ListLabels
module Ut = Ppx_deriving_utils
module Ty = Ut.Type
open Ppxlib
open Asttypes
open Parsetree
open Ast_builder.Default

let deriver = "mk_record"

let impl_make_record ~ptype_name ~labels constructor =
  let { txt = name; _ } = constructor.pcd_name in
  let loc = constructor.pcd_loc in
  let new_name = Ut.make_let_name name ~ptype_name in
  let new_args = Ut.make_arguments constructor.pcd_args in
  let new_recr = Ut.make_record ~loc new_args in
  let new_variant = Ut.make_variant ~loc name new_recr in
  let new_lambda = Ut.make_func ~loc ~labels new_args new_variant in
  Ut.make_let loc
    ~name:(ppat_var ~loc { loc; txt = new_name })
    ~value:new_lambda

let intf_make_record ~ptype_name ~labels constructor =
  let { txt = name; _ } = constructor.pcd_name in
  let loc = constructor.pcd_loc in
  let new_name = Ut.make_let_name name ~ptype_name in
  let new_args = Ut.make_arguments constructor.pcd_args in
  let tyreturn = Ty.make_const ~loc ptype_name.txt in
  Ty.make_sig loc
    ~ty:(Ty.make_arrow ~loc ~labels new_args tyreturn)
    ~name:new_name

let generate_impl ~ctxt (_rec_flag, type_declarations) labels =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let aux (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract; ptype_loc; _ }
    | { ptype_kind = Ptype_record _; ptype_loc; _ }
    | { ptype_kind = Ptype_open; ptype_loc; _ } ->
        let ext = Location.error_extensionf ~loc:ptype_loc "[todo]" in
        [ Ast_builder.Default.pstr_extension ~loc ext [] ]
    | { ptype_kind = Ptype_variant decls; ptype_name; _ } ->
        List.map decls ~f:(impl_make_record ~ptype_name ~labels)
  in
  List.map type_declarations ~f:aux |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) labels =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let aux (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract; ptype_loc; _ }
    | { ptype_kind = Ptype_record _; ptype_loc; _ }
    | { ptype_kind = Ptype_open; ptype_loc; _ } ->
        let ext = Location.error_extensionf ~loc:ptype_loc "[todo]" in
        [ Ast_builder.Default.psig_extension ~loc ext [] ]
    | { ptype_kind = Ptype_variant decls; ptype_name; _ } ->
        List.map decls ~f:(intf_make_record ~ptype_name ~labels)
  in
  List.map type_declarations ~f:aux |> List.concat

let str_type_decl =
  let args = Ppxlib.Deriving.Args.(empty +> flag "labels") in
  let deriver ~ctxt (rec_flag, type_declarations) labels =
    generate_impl ~ctxt (rec_flag, type_declarations) labels
  in
  Ppxlib.Deriving.Generator.V2.make args deriver

let sig_type_decl =
  let args = Ppxlib.Deriving.Args.(empty +> flag "labels") in
  let deriver ~ctxt (rec_flag, type_declarations) labels =
    generate_intf ~ctxt (rec_flag, type_declarations) labels
  in
  Ppxlib.Deriving.Generator.V2.make args deriver

let my_deriver = Deriving.add deriver ~str_type_decl ~sig_type_decl