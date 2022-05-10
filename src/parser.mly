%{
  open Ast
%}

%token <int> IMM
%token <int> PARAM
%token LPAREN
%token RPAREN
%token MUL
%token DIV
%token ADD
%token SUB
%token EOF

%left ADD SUB
%left MUL DIV

%start <Ast.expr> prog
%%

prog:
  | e = expr ; EOF { e } ;

expr:
  | LPAREN e = expr RPAREN        { e }
  | e = IMM                       { Imm e }
  | e = PARAM                     { Param e }
  | a = expr op = bin_op b = expr { Binary (op, a, b) } ;

%inline bin_op:
  | ADD { Add }
  | SUB { Sub }
  | MUL { Mul }
  | DIV { Div } ;