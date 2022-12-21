module List = ListLabels
open Ppxlib
open Ast_builder.Default

let make_location txt loc = { txt; loc }
let make_nolocation txt = make_location txt !Ast_helper.default_loc
let make_lid s = make_nolocation (Longident.parse s)

let make_arguments = function
  | Pcstr_record fields ->
      List.map fields ~f:(fun f -> (f.pld_name, f.pld_type))
  | _ -> failwith "todo"

let make_record ~loc fields =
  let fields = List.map fields ~f:(fun (n, _) -> (n.txt, evar n.txt)) in
  let fields = List.map fields ~f:(fun (n, v) -> (make_lid n, v ~loc)) in
  pexp_record ~loc fields None

let make_label ~labels name = if labels then Labelled name.txt else Nolabel

let make_func ~loc ~labels args body =
  let aux (arg, _) body =
    pexp_fun ~loc (make_label ~labels arg) None (ppat_var ~loc arg) body
  in
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

module Type = struct
  let make_const ~loc name = ptyp_constr ~loc { loc; txt = lident name } []

  let make_arrow ~loc ~labels params return =
    let _ = labels in
    let aux (label, ty) ret =
      ptyp_arrow ~loc (make_label ~labels label) ty ret
    in
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