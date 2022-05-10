open IR

type t = { output : out_channel
         ; mutable stack : X86assembly.address list
         ; mutable registers : X86assembly.register list }

let make output =
  { output
  ; stack = []
  ; registers = [ R8 ; R9 ; R10 ; R11 ] }

module Register = struct
  let alloc state =
    match state.registers with
    | reg :: available ->
      state.registers <- available;
      reg
    | [] -> failwith "No register available"

  let free state reg =
    match reg with
    | X86assembly.REG reg when not (List.mem reg state.registers) ->
      state.registers <- reg :: state.registers
    | _ -> ()
end

module Stack = struct
  let push state addr =
    state.stack <- addr :: state.stack

  let drop state n =
    let rec aux res stack n =
      match stack with
      | top :: stack ->
        if n = 0 then List.append (List.rev res) stack
        else aux (top :: res) stack (n - 1)
      | [] -> failwith "Empty stack"
    in
    state.stack <- aux [] state.stack n
end

let rec range start end_ =
  if start > end_ then []
  else start :: range (start + 1) end_

let compile_params out ir =
  let params =
    List.filter_map
      (function PARAM x -> Some x | _ -> None)
      ir
  in
  let stack_size =
    (List.fold_left Int.max 0 params) + 1
  in
  X86assembly.emit_list out
    [ PUSH (REG RBP)
    ; MOV (REG RSP, REG RBP)
    ; SUB (IMM (stack_size * 8), REG RSP)
    ; MOV (REG RSI, STACK stack_size) ];
  List.iter
    (fun idx ->
      X86assembly.emit_list out
        [ MOV (STACK stack_size, REG RAX)
        ; MOV (IND (Int ((stack_size - idx) * 8), RAX), REG RDI)
        ; CALL "_atoi"
        ; MOV (REG RAX, STACK (stack_size - idx))])
    (range 1 (stack_size - 1));
  stack_size

let restore_stack out size =
  X86assembly.emit_list out
    [ ADD (IMM (size * 8), REG RSP)
    ; POP (REG RBP) ]

let resolve_stack_to_stack state src dst =
  match src, dst with
  | X86assembly.STACK _, X86assembly.STACK _ ->
    let reg = Register.alloc state in
    X86assembly.emit state.output (MOV (src, REG reg));
    X86assembly.REG reg
  | _ -> src

let emit_print_address output addr =
  X86assembly.emit_list output
    [ MOV (addr, REG RSI)
    ; LEA (Label "FORMAT", RIP, REG RDI)
    ; CALL "_printf" ]

let rec compile ?stack_size state ir =
  match ir, state.stack with
  | PROC (name, body), _ ->
    X86assembly.emit_global state.output name;
    let stack_size = compile_params state.output body in
    List.iter (compile ~stack_size state) body
  | INT value, _ ->
    let reg = Register.alloc state in
    X86assembly.emit state.output (MOV (IMM value, REG reg));
    Stack.push state (X86assembly.REG reg)
  | PRINT, top :: _ ->
    emit_print_address state.output top;
    Register.free state top;
    Stack.drop state 0
  | RETURN, a :: _ ->
    Option.iter (restore_stack state.output) stack_size;
    X86assembly.emit_ret state.output a;
    Register.free state a;
    Stack.drop state 0
  | ADD, a :: b :: _ ->
    let a = resolve_stack_to_stack state a b in
    X86assembly.emit state.output (ADD (a, b));
    Register.free state a;
    Stack.drop state 0
  | MUL, a :: b :: _ ->
    let a = resolve_stack_to_stack state a b in
    X86assembly.emit state.output (IMUL (a, b));
    Register.free state a;
    Stack.drop state 0
  | SUB, a :: b :: _ ->
    let b = resolve_stack_to_stack state b a in
    X86assembly.emit state.output (SUB (b, a));
    Register.free state b;
    Stack.drop state 1
  | PARAM idx, _ ->
    Stack.push state (STACK idx)
  | ir, stack ->
    Format.eprintf "Error, unexpected: %a. Current stack:\n" IR.pp ir;
    List.iter (fun a -> Format.eprintf "\t%a\n" X86assembly.pp_address a)
      stack;
    assert false

let compile state ir =
  List.iter (compile state) ir;
  X86assembly.emit_string state.output "FORMAT" "%d\\n"