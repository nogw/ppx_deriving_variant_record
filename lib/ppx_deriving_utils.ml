module List = ListLabels
open Ppxlib
open Ast_builder.Default

let make_location txt loc = { txt; loc }
let make_nolocation txt = make_location txt !Ast_helper.default_loc
let make_lid s = make_nolocation (Longident.parse s)

let make_arguments = function
  | Pcstr_record fields -> List.map fields ~f:(fun field -> (field.pld_name, field.pld_type))
  | Pcstr_tuple _ -> failwith "Tuples are not supported"

let make_record ~loc fields =
  fields
  |> List.map ~f:(fun (n, _) -> (n.txt, evar n.txt))
  |> List.map ~f:(fun (n, v) -> (make_lid n, v ~loc))
  |> fun fields -> pexp_record ~loc fields None

let make_label ~labels name = if labels then Labelled name.txt else Nolabel

let make_func ~loc ~labels args body =
  let args = fst (List.split args) in
  let aux arg body = pexp_fun ~loc (make_label ~labels arg) None (ppat_var ~loc arg) body
  in List.fold_right ~f:aux ~init:body args

let make_variant ~loc name body =
  pexp_construct ~loc { loc; txt = Lident name } (Some body)

let make_let_name ~ptype_name ~prefix name =
  let name = String.lowercase_ascii name in
  match prefix with 
  | true -> Printf.sprintf "%s_%s" ptype_name.txt name
  | false -> Printf.sprintf "%s" name  

let make_let ~name ?(pvb_attributes = []) ~value loc =
  pstr_value ~loc Nonrecursive
    [ { pvb_pat = name; pvb_expr = value; pvb_attributes; pvb_loc = loc } ]

let make_const ~loc name = ptyp_constr ~loc { loc; txt = lident name } []

let make_arrow ~loc ~labels params return =
  let aux (label, ty) ret = ptyp_arrow ~loc (make_label ~labels label) ty ret
  in List.fold_right ~f:aux ~init:return params

let make_sig ~name ~ty ?(pval_attributes = []) ?(pval_prim = []) loc =
  psig_value ~loc
    {
      pval_loc = loc;
      pval_name = { loc; txt = name };
      pval_type = ty;
      pval_attributes;
      pval_prim;
    }