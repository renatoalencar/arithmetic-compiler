open IR

type t = { output : out_channel
         ; mutable buffer : X86assembly.instruction list list
         ; mutable stack : X86assembly.address list
         ; mutable registers : X86assembly.register list }

let make output =
  { output
  ; buffer    = []
  ; stack     = []
  ; registers = [ RAX ; RCX ; RDX ; R8 ] }

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
    let rec aux stack n =
      match stack, n with
      | _ :: stack, 0 -> stack
      | _ :: stack, n -> aux stack (n - 1)
      | [], _ -> failwith "Empty stack"
    in
    state.stack <- aux state.stack (n - 1)
end

module InstrBuffer = struct
  let push state instructions =
    state.buffer <- instructions :: state.buffer

  let flush state =
    let rec aux = function
      | [] -> ()
      | instructions :: buffer ->
        aux buffer;
        X86assembly.emit_list state.output instructions
    in
    aux state.buffer
end

let rec range start end_ =
  if start > end_ then []
  else start :: range (start + 1) end_

let compile_params state ir =
  let params =
    List.filter_map
      (function PARAM x -> Some x | _ -> None)
      ir
  in
  let stack_size =
    (List.fold_left Int.max 0 params) + 1
  in
  InstrBuffer.push state
    [ PUSH (REG RBP)
    ; MOV (REG RSP, REG RBP)
    ; SUB (IMM (stack_size * 8), REG RSP)
    ; MOV (REG RSI, STACK stack_size) ];
  List.iter
    (fun idx ->
      InstrBuffer.push state
        [ MOV (STACK stack_size, REG RAX)
        ; MOV (IND (Int ((stack_size - idx) * 8), RAX), REG RDI)
        ; CALL "_atoi"
        ; MOV (REG RAX, STACK (stack_size - idx))])
    (range 1 (stack_size - 1));
  stack_size

let restore_stack state size =
  InstrBuffer.push state
    [ ADD (IMM (size * 8), REG RSP)
    ; POP (REG RBP) ]

let resolve_parameters state src dst =
  match src, dst with
  | X86assembly.STACK _, X86assembly.STACK _ ->
    let reg = Register.alloc state in
    InstrBuffer.push state [ MOV (src, REG reg) ];
    X86assembly.REG reg, dst
  | _, X86assembly.IMM _ ->
    let reg = Register.alloc state in
    InstrBuffer.push state [ MOV (dst, REG reg) ];
    src, X86assembly.REG reg
  | _ -> src, dst

let push_print_address state addr =
  InstrBuffer.push state
    [ MOV (addr, REG RSI)
    ; LEA (Label "FORMAT", RIP, REG RDI)
    ; CALL "_printf" ]

let rec compile ?stack_size state ir =
  match ir, state.stack with
  | PROC (name, body), _ ->
    X86assembly.emit_global state.output name;
    let stack_size = compile_params state body in
    List.iter (compile ~stack_size state) body
  | INT value, _ ->
    Stack.push state (X86assembly.IMM value)
  | PRINT, top :: _ ->
    push_print_address state top;
    Register.free state top;
    Stack.drop state 1
  | RETURN, a :: _ ->
    Option.iter (restore_stack state) stack_size;
    InstrBuffer.push state @@ X86assembly.return a;
    Register.free state a;
    Stack.drop state 1
  | ADD, a :: b :: _ ->
    let a, b = resolve_parameters state a b in
    InstrBuffer.push state [ ADD (a, b) ];
    Register.free state a;
    Stack.drop state 2;
    Stack.push state b
  | MUL, a :: b :: _ ->
    let a, b = resolve_parameters state a b in
    InstrBuffer.push state [ IMUL (a, b) ];
    Register.free state a;
    Stack.drop state 2;
    Stack.push state b
  | SUB, a :: b :: _ ->
    let b, a = resolve_parameters state b a in
    InstrBuffer.push state [ SUB (b, a) ];
    Register.free state b;
    Stack.drop state 2;
    Stack.push state a
  | PARAM idx, _ ->
    Stack.push state (STACK idx)
  | ir, stack ->
    Format.eprintf "Error, unexpected: %a. Current stack:\n" IR.pp ir;
    List.iter (fun a -> Format.eprintf "\t%a\n" X86assembly.pp_address a)
      stack;
    assert false

let compile state ir =
  List.iter (compile state) ir;
  InstrBuffer.flush state;
  X86assembly.emit_string state.output "FORMAT" "%d\\n"