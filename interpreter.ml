open Ast

let rec eval_expr env = function
  | Var x -> string_of_int (List.assoc x env)
  | Int n -> string_of_int n
  | Str s -> s
  | Binop (Add, e1, e2) -> string_of_int (int_of_string (eval_expr env e1) + int_of_string (eval_expr env e2))
  | Binop (Sub, e1, e2) -> string_of_int (int_of_string (eval_expr env e1) - int_of_string (eval_expr env e2))
  | Binop (Mul, e1, e2) -> string_of_int (int_of_string (eval_expr env e1) * int_of_string (eval_expr env e2))
  | Binop (Div, e1, e2) -> string_of_int (int_of_string (eval_expr env e1) / int_of_string (eval_expr env e2))

let eval_relop env r e1 e2 =
  let v1 = int_of_string (eval_expr env e1) in
  let v2 = int_of_string (eval_expr env e2) in
  match r with
  | Greater -> v1 > v2
  | Less -> v1 < v2
  | GreaterEqual -> v1 >= v2
  | LessEqual -> v1 <= v2
  | Equal -> v1 = v2
  | NotEqual -> v1 <> v2

let rec find_line n = function
  | [] -> failwith (Printf.sprintf "Line %d not found" n)
  | Line (num, instr) :: _ when num = n -> instr
  | _ :: lines -> find_line n lines

let format_output result = 
  "Resultat : [" ^ (String.concat ", " result) ^ "]"

let rec eval_instr env lines output = function
  | Print exprs -> 
    let result = List.map (eval_expr env) exprs in
    let new_output = output @ result in
    env, lines, new_output
  | If (e1, r, e2, i) -> 
    if eval_relop env r e1 e2 then eval_instr env lines output i else env, lines, output
  | Goto e -> 
    let line_num = int_of_string (eval_expr env e) in
    env, line_num :: lines, output
  | End -> env, [], output
  | Rem _ -> env, lines, output
  | Newline -> env, lines, output @ ["\n"]
  | Assign (v, e) -> 
    let value = int_of_string (eval_expr env e) in
    (v, value) :: env, lines, output

let rec eval_program env program lines output =
  match lines with
  | [] -> output
  | line_num :: remaining_lines ->
    let instr = find_line line_num program in
    let env', lines', new_output = eval_instr env remaining_lines output instr in
    (* trouve la ligne suivante si aucun saut n'a ete fait*)
    let next_lines = if lines' = remaining_lines then 
      match List.find_opt (fun (Line (num, _)) -> num > line_num) program with
      | Some (Line (next_num, _)) -> next_num :: lines'
      | None -> lines'
    else lines' in
    eval_program env' program next_lines new_output

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Lexer.token lexbuf in
  let sorted_program = List.sort (fun (Line (num1, _)) (Line (num2, _)) -> compare num1 num2) program in
  match sorted_program with
  | [] -> failwith "Empty program"
  | Line (first_line_num, _) :: _ ->
    let result = eval_program [] sorted_program [first_line_num] [] in
    print_endline (String.concat "\n" result)
