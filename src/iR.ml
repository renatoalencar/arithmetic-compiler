type t =
  | PROC of string * t list
  | RETURN
  | INT of int
  | PRINT
  | ADD
  | SUB
  | MUL
  | PARAM of int
  [@@deriving show]

open Ast
let compile_op op =
  match op with
  | Add -> ADD
  | Sub -> SUB
  | Mul -> MUL
  | Div -> failwith "Div not supported yet"

let rec compile_expr expr ir =
  match expr with
  | Imm value -> INT value :: ir
  | Param idx -> PARAM idx :: ir
  | Binary (op, a, b) ->
    (compile_op op) :: compile_expr a (compile_expr b ir)

let compile ast =
  let body =
    List.rev_append
      (compile_expr ast [])
      [ PRINT; INT 0; RETURN ]
  in
  [ PROC ("_main", body) ]
