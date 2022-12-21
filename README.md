# [*WIP*][@@deriving mk_record]

deriving Mk_record is a [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) plugin that generates a constructor for variant records, Given a variant record, a function is generated that accepts all fields as arguments or labeled arguments

| NOTE: ppx_deriving is currently only a test to understand a little more of ppx tool development and for personal use, so I do not recommend to use it in a real project, many problems still exist and have not been tested

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