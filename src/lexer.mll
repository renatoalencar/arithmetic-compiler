{
open Parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let imm = '-'? digit+
let whitespace = [' ' '\t']+
let newline = '\r'? '\n'

rule read =
  parse
  | whitespace  { read lexbuf }
  | newline     { Lexing.new_line lexbuf; read lexbuf }
  | imm         { IMM (int_of_string (Lexing.lexeme lexbuf))}
  | '$'         { PARAM (int_of_string (read_param lexbuf)) }
  | '+'         { ADD }
  | '-'         { SUB }
  | '*'         { MUL }
  | '/'         { DIV }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof         { EOF }
and read_param = parse
  | '$'     { read_param lexbuf }
  | digit+  { Lexing.lexeme lexbuf }