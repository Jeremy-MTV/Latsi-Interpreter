type program = line list
and line = Line of int * instr
and instr =
  | Print of expr list
  | If of expr * relop * expr * instr
  | Goto of expr
  | End
  | Rem of string
  | Newline
  | Assign of string * expr

and expr =
  | Var of string
  | Int of int
  | Str of string
  | Binop of binop * expr * expr

and binop =
  | Add
  | Sub
  | Mul
  | Div

and relop =
  | Greater
  | Less
  | GreaterEqual
  | LessEqual
  | Equal
  | NotEqual
