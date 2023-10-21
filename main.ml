open Int64

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


  (* 定义 completion 函数 *)
let completion bool_list n =
  (* 获取列表长度 *)
  let list_length = List.length bool_list in

  (* 如果 n 大于列表长度，扩展列表 *)
  if n > list_length then
    (* 计算需要扩展的元素数量 *)
    let rec extend_list remaining current_list =
      if remaining > 0 then
        extend_list (remaining - 1) (current_list @ [false]) (* 添加 false 到列表末尾 *)
      else
        current_list
    in
    extend_list (n - list_length) bool_list (* 开始扩展列表 *)

  (* 如果 n 小于列表长度，切割列表 *)
  else if n < list_length then
    let rec cut_list i = function
      | [] -> [] (* 如果列表为空或者我们已经取出需要的元素，则返回空列表 *)
      | h :: t when i < n -> h :: cut_list (i + 1) t (* 如果 i < n，继续递归 *)
      | _ -> [] (* 其他情况，返回空列表结束递归 *)
    in
    cut_list 0 bool_list (* 开始切割列表 *)

  (* 如果 n 等于列表长度，直接返回原列表 *)
  else
    bool_list

let int64_list = [0L; Int64.shift_left 1L 36];;
let result = process_int64_list int64_list;;
let result_str = string_of_bool_list result;;
Printf.printf "%s\n\n" result_str;;


let new_result = completion result 4;;
let nnew_result = string_of_bool_list new_result;;
Printf.printf "%s\n\n" nnew_result;;

let more_result = completion result 105;;
let mmore_result = string_of_bool_list more_result;;
Printf.printf "%s\n\n" mmore_result;;

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




open Int64

(* 生成指定位数的随机正数 int64 *)
let gen_random_int64 bits =
  let rec aux acc bits_remaining =
    if bits_remaining <= 0 then acc
    else
      (* 生成随机位并左移累加 *)
      let bit = if Random.bool () then 1L else 0L in
      let new_acc = logor (shift_left acc 1) bit in
      aux new_acc (bits_remaining - 1)
  in
  if bits >= 64 then
    (* 如果需要生成完整的64位数，确保最高位是1 *)
    aux (shift_left 1L 63) 63
  else
    (* 否则生成少于64位的数 *)
    aux 0L bits

(* 根据 n 生成一个 int64 列表 *)
let gen_alea n =
  let rec aux n acc =
    if n <= 0 then acc
    else if n < 64 then
      (* 对于 n < 64，生成一个最大位数为 n 的随机数 *)
      let rand_num = gen_random_int64 n in
      rand_num :: acc
    else
      (* 对于 n >= 64，生成一个64位随机数并处理剩下的位数 *)
      let rand_num = gen_random_int64 64 in
      aux (n - 64) (rand_num :: acc)
  in
  Random.self_init ();  (* 初始化随机数种子 *)
  List.rev (aux n [])   (* 生成随机数列表并反转 *)

(* 测试 gen_alea 函数 *)

let ex_result = gen_alea 100;;
  (* 打印结果 *)
List.iter (fun i -> Printf.printf "%Ld\n" i) ex_result;;

let exf_result  = process_int64_list ex_result;;
let exff_result = string_of_bool_list exf_result;;

Printf.printf "%s\n\n" exff_result;;


