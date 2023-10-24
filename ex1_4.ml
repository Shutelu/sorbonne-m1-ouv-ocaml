(* 定义 split_at 函数 *)
let split_at (lst : 'a list) (n : int) : 'a list * 'a list =
  let rec aux i acc = function
    | [] -> (List.rev acc, []) 
    | h :: t as l -> if i = 0 then (List.rev acc, l)
                     else aux (i-1) (h :: acc) t 
  in
  if n <= 0 then ([], lst) 
  else aux n [] lst;;

(* 将 bool list 转换为 int64 *)
let bool_list_to_int64 bool_list =
  List.fold_left (fun acc b -> Int64.(add (shift_left acc 1) (if b then 1L else 0L))) 0L bool_list;;

(* 将参数列表分割为多个子列表，每个子列表最多64位 *)
let rec split_list list =
  match list with
  | [] -> []
  | _  -> 
      let (sous_list, rest) = split_at list 64 in
      sous_list :: split_list rest;;

(* 转换每个子列表并将结果收集到结果列表中 *)
let process_sublists sublist =
  bool_list_to_int64 (List.rev sublist);;

(* 定义 composition 函数 *)
let composition (para_list: bool list) : int64 list =
  let splitted_lists = split_list para_list in
  List.map process_sublists splitted_lists;;

(* 测试 composition 函数 *)


let composition_result = composition result;;
List.iter (fun i -> Printf.printf "%Ld\n" i) composition_result;;

let print_int64_list list =
  let string_of_int64_elems elems =
    let strs = List.map (fun i -> Printf.sprintf "%Ld" i) elems in
    String.concat "; " strs
  in
  Printf.printf "[%s]\n" (string_of_int64_elems list);;

print_int64_list composition_result;;

