module List = ListLabels
open Ppxlib
open Ast_builder.Default

let make_location txt loc = { txt; loc }
let make_nolocation txt = make_location txt !Ast_helper.default_loc
let make_lid s = make_nolocation (Longident.parse s)

let make_arguments = function
  | Pcstr_record fields ->
      fields
      |> List.map ~f:(fun field -> (field.pld_name, field.pld_type))
      |> List.split
  | _ -> failwith "todo"

let make_record ~loc fields =
  let fields = List.map fields ~f:(fun { txt; _ } -> (txt, evar txt)) in
  let fields = List.map fields ~f:(fun (n, v) -> (make_lid n, v ~loc)) in
  pexp_record ~loc fields None

let make_func ~loc args body =
  let aux arg body = pexp_fun ~loc Nolabel None (ppat_var ~loc arg) body in
  List.fold_right ~f:aux ~init:body args

let make_variant ~loc name body =
  pexp_construct ~loc { loc; txt = Lident name } (Some body)

let make_let_name ~ptype_name name =
  let { txt; _ } = ptype_name in
  let name = String.lowercase_ascii name in
  Printf.sprintf "mk_%s_%s" txt name

let make_let ~name ?(pvb_attributes = []) ~value loc =
  pstr_value ~loc Nonrecursive
    [ { pvb_pat = name; pvb_expr = value; pvb_attributes; pvb_loc = loc } ]

let get_bool expr =
  match expr with
  | Pexp_construct ({ txt = Lident "true"; _ }, _) -> Ok true
  | Pexp_construct ({ txt = Lident "false"; _ }, _) -> Ok false
  | _ -> Error "boolean"

module Type = struct
  let make_const ~loc name = ptyp_constr ~loc { loc; txt = lident name } []

  let make_arrow ~loc params return =
    let aux p ret = ptyp_arrow ~loc Nolabel (make_const ~loc p.txt) ret in
    List.fold_right ~f:aux ~init:return params

  let make_sig ~name ~ty ?(pval_attributes = []) ?(pval_prim = []) loc =
    psig_value ~loc
      {
        pval_loc = loc;
        pval_name = { loc; txt = name };
        pval_type = ty;
        pval_attributes;
        pval_prim;
      }
end