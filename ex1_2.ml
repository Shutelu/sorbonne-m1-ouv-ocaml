(* 将一个 int64 转换为一个 bool 列表，表示其二进制形式 *)
let decomposition n =
  let rec aux n acc i =
    if i < 0 then acc
    else aux n ((Int64.logand n (Int64.shift_left 1L i) <> 0L) :: acc) (i - 1)
  in
  aux n [] 63;;

(* 找到最后一个 true 在 bool 列表中的索引 *)
let rec find_last_true_index lst idx last_true =
  match lst with
  | [] -> last_true
  | h::t -> 
      let new_last = if h then idx else last_true in
      find_last_true_index t (idx + 1) new_last;;

(* 裁剪列表，仅包括最后一个 true 之前的所有项（包括它） *)
let trim_list lst =
  let last_true_pos = find_last_true_index lst 0 (-1) in
  if last_true_pos = -1 then []
  else List.rev (List.fold_left (fun acc h -> if (List.length acc) <= last_true_pos then h::acc else acc) [] lst);;

(* 处理 int64 list，并按指定格式打印 *)
let process_int64_list (int64_list: int64 list) =
  let list_length = List.length int64_list in
  let process_single_int64 i n =
    let bits = decomposition n in
    (* 如果这不是列表中的最后一个元素，则打印完整的列表，否则我们裁剪列表 *)
    let final_bits = if i < list_length - 1 then bits else trim_list bits in
    final_bits
  in
  let result_list = List.mapi process_single_int64 int64_list in
  List.flatten result_list;;

(* 将 bool 列表转换为字符串表示 *)
let string_of_bool_list lst =
  let rec aux lst acc =
    match lst with
    | [] -> acc ^ "]"
    | [h] -> acc ^ (string_of_bool h) ^ "]"
    | h::t -> aux t (acc ^ (string_of_bool h) ^ "; ")
  in
  aux lst "[";;

let int64_list = [0L; Int64.shift_left 1L 36];;
let result = process_int64_list int64_list;;
let result_str = string_of_bool_list result;;
Printf.printf "%s\n" result_str;;