let emit_assembly_file filename ir =
  let out = open_out filename in
  let compiler = Compiler.make out in
  Compiler.compile compiler ir;
  close_out out

let compile_assembly_file filename =
  let cflags = Option.value ~default:"" (Sys.getenv_opt "CFLAGS") in
  let cc = Option.value ~default:"cc" (Sys.getenv_opt "CC") in
  let command = String.concat " " [cc ; cflags ; filename]  in
  assert (Sys.command command = 0)

let parse filename =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let ast = Parser.prog Lexer.read lexbuf in
  In_channel.close input;
  ast

let compile filename =
  let ast = parse filename in
  let ir = IR.compile ast in
  let output_assembly_file = Filename.remove_extension filename ^ ".s" in
  emit_assembly_file output_assembly_file ir;
  compile_assembly_file output_assembly_file

let () =
  compile Sys.argv.(1)