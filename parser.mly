%{
  open Ast
%}

%token <string> VAR
%token <int> INT
%token <string> STRING
%token IMPRIME SI ALORS VAVERS FIN REM NL EQUAL NEQ GT LT GEQ LEQ PLUS MINUS MULT DIV COMMA EOF LPAREN RPAREN

%start <Ast.program> program
%type <Ast.line> line
%type <Ast.instr> instr

%%

program:
  | EOF { [] }
  | lines EOF { $1 }

lines:
  | line { [$1] }
  | line lines { $1 :: $2 }

line:
  | INT instr { Line ($1, $2) }

instr:
  | IMPRIME expr_list { Print ($2) }
  | SI expr relop expr ALORS instr { If ($2, $3, $4, $6) }
  | VAVERS expr { Goto ($2) }
  | FIN { End }
  | REM STRING { Rem ($2) }
  | NL { Newline }
  | VAR EQUAL expr { Assign ($1, $3) }

expr_list:
  | expr { [$1] }
  | expr COMMA expr_list { $1 :: $3 }

expr:
  | term { $1 }
  | expr PLUS term { Binop (Add, $1, $3) }
  | expr MINUS term { Binop (Sub, $1, $3) }

term:
  | factor { $1 }
  | term MULT factor { Binop (Mul, $1, $3) }
  | term DIV factor { Binop (Div, $1, $3) }

factor:
  | VAR { Var $1 }
  | INT { Int $1 }
  | STRING { Str $1 }
  | LPAREN expr RPAREN { $2 }

relop:
  | GT { Greater }
  | LT { Less }
  | GEQ { GreaterEqual }
  | LEQ { LessEqual }
  | EQUAL { Equal }
  | NEQ { NotEqual }
