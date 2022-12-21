# [*WIP*][@@deriving mk_record]

deriving Mk_record is a [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) plugin that generates a constructor for variant records, Given a variant record, a function is generated that accepts all fields as arguments or labeled arguments

## Example
```ocaml
type t =
  | Num of { value : int }
  | Bop of { operator : string; left : t; right : t }
  [@@deriving mk_record]

let normal =
  Bop
    {
      operator = "-";
      left = Num { value = 1 };
      right =
        Bop
          {
            operator = "+";
            left = Num { value = 2 };
            right = Num { value = 3 };
          };
    }

(* mk_record generate the functions below *)
let short = bop "-" (num 1) (bop "+" (num 2) (num 3))
```