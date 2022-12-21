module Ut = Ppx_deriving_utils
module List = ListLabels
open Ppxlib
open Ast_builder.Default

let deriver = "mk_record"

let impl_make_record ~ptype_name (constructor : constructor_declaration) =
  let { txt = name; _ } = constructor.pcd_name in
  let loc = constructor.pcd_loc in
  let new_name = Ut.make_let_name name ~ptype_name in
  let new_args = fst (Ut.make_arguments constructor.pcd_args) in
  let new_recr = Ut.make_record ~loc new_args in
  let new_variant = Ut.make_variant ~loc name new_recr in
  let new_lambda = Ut.make_func ~loc new_args new_variant in
  Ut.make_let loc
    ~name:(ppat_var ~loc { loc; txt = new_name })
    ~value:new_lambda

let intf_make_record ~ptype_name (constructor : constructor_declaration) =
  let { txt = name; _ } = constructor.pcd_name in
  let loc = constructor.pcd_loc in
  let new_name = Ut.make_let_name name ~ptype_name in
  let new_args = fst (Ut.make_arguments constructor.pcd_args) in
  Ut.make_sig loc
    ~ty:(Ut.make_arrow ~loc new_args (Ut.make_const ~loc ptype_name.txt))
    ~name:new_name

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let aux (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract; ptype_loc; _ }
    | { ptype_kind = Ptype_record _; ptype_loc; _ }
    | { ptype_kind = Ptype_open; ptype_loc; _ } ->
        let ext = Location.error_extensionf ~loc:ptype_loc "[todo]" in
        [ Ast_builder.Default.pstr_extension ~loc ext [] ]
    | { ptype_kind = Ptype_variant decls; ptype_name; _ } ->
        List.map decls ~f:(impl_make_record ~ptype_name)
  in
  List.map type_declarations ~f:aux |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let aux (td : type_declaration) =
    match td with
    | { ptype_kind = Ptype_abstract; ptype_loc; _ }
    | { ptype_kind = Ptype_record _; ptype_loc; _ }
    | { ptype_kind = Ptype_open; ptype_loc; _ } ->
        let ext = Location.error_extensionf ~loc:ptype_loc "[todo]" in
        [ Ast_builder.Default.psig_extension ~loc ext [] ]
    | { ptype_kind = Ptype_variant decls; ptype_name; _ } ->
        List.map decls ~f:(intf_make_record ~ptype_name)
  in
  List.map type_declarations ~f:aux |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf

let my_deriver =
  Deriving.add deriver ~str_type_decl:impl_generator
    ~sig_type_decl:intf_generator