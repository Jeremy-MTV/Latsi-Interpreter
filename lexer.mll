{
  open Parser
  exception Lexing_error of string
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf } (* skip whitespace *)
  | "IMPRIME" { IMPRIME }
  | "SI" { SI }
  | "ALORS" { ALORS }
  | "VAVERS" { VAVERS }
  | "FIN" { FIN }
  | "REM" { REM }
  | "NL" { NL }
  | ['A'-'Z'] as c { VAR (Char.escaped c) }
  | ['0'-'9']+ as l { INT (int_of_string l) }
  | "\"" [^'"']* "\"" as s { STRING (String.sub s 1 (String.length s - 2)) }
  | "=" { EQUAL }
  | "<>" | "><" { NEQ }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULT }
  | "/" { DIV }
  | ',' { COMMA }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ as c { raise (Lexing_error (Printf.sprintf "Unexpected character: %c" c)) }
