type register =
  | RIP
  | RDI
  | RSI
  | RAX
  | RBX
  | RCX
  | RDX
  | RBP
  | RSP
  | R8
  | R9
  | R10
  | R11
  [@@deriving show]

let register_to_string = function
  | RAX -> "%rax"
  | RBX -> "%rbx"
  | RCX -> "%rcx"
  | RDX -> "%rdx"
  | RIP -> "%rip"
  | RDI -> "%rdi"
  | RSI -> "%rsi"
  | R8  -> "%r8"
  | R9  -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | RBP -> "%rbp"
  | RSP -> "%rsp"

type offset =
  | Label of string
  | Int of int
  [@@deriving show]

type address =
  | REG of register
  | IND of offset * register
  | LBL of string
  | STACK of int
  | IMM of int
  [@@deriving show]

type instruction =
  | MOV of address * address
  | LEA of offset * register * address
  | CALL of string
  | ADD of address * address
  | SUB of address * address
  | IMUL of address * address
  | RET
  | PUSH of address
  | POP of address
  | PSEUDO of string
  | LABEL of string

let string_of_address address =
  match address with
  | REG reg -> register_to_string reg
  | IND (offset, reg) ->
    let offset =
      match offset with
      | Label lbl -> lbl
      | Int off -> string_of_int off
    in
    Printf.sprintf "%s(%s)" offset (register_to_string reg)
  | LBL vl -> vl
  | STACK offset -> Printf.sprintf "%d(%%rsp)" (offset * 8)
  | IMM value -> Printf.sprintf "$%d" value

let emit_op2 output op src dst =
  Printf.fprintf output
    "    %s\t%s, %s"
    op
    (string_of_address src)
    (string_of_address dst)

let emit_op1 output op p =
  Printf.fprintf output
    "    %s\t%s"
    op
    (string_of_address p)

let emit_op0 output op =
  Printf.fprintf output "    %s" op 

let emit output instr =
  begin match instr with
  | MOV (src, dst) ->
    emit_op2 output "movq" src dst
  | LEA (offset, src, dst) ->
    emit_op2 output "leaq" (IND (offset, src)) dst
  | CALL label ->
    emit_op1 output "callq" (LBL label)
  | ADD (src, dst) ->
    emit_op2 output "addq" src dst
  | SUB (src, dst) ->
    emit_op2 output "subq" src dst
  | IMUL (src, dst) ->
    emit_op2 output "imulq" src dst
  | RET ->
    emit_op0 output "retq"
  | PUSH address ->
    emit_op1 output "pushq" address
  | POP address ->
    emit_op1 output "popq" address

  | PSEUDO pseudo ->
    Printf.fprintf output "    %s" pseudo
  | LABEL label ->
    Printf.fprintf output "%s:" label
  end;
  Printf.fprintf output "\r\n"

let emit_list output list =
  List.iter (emit output) list

let emit_global output label =
  emit_list output
    [ PSEUDO (".globl " ^ label)
    ; LABEL label ]

let emit_string output label value =
  emit_list output
    [ LABEL label
    ; PSEUDO (".asciz \"" ^ value ^ "\"")]

let return value =
  [ MOV (value, REG RAX)
  ; RET ]