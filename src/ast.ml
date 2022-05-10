type operator = Add | Sub | Mul | Div

type expr =
  | Imm of int
  | Param of int
  | Binary of operator * expr * expr