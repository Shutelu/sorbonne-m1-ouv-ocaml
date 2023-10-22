open Int64

(* Q1.2 |
  description : utiliser par decomposition, retourne le binaire de n:int64 sous forme de liste boolean
  paremetre n : element int64 pour obtenir le binaire
  return : liste boolean
*)
let decomposition_aux n =
  let rec aux tmp_lst currentBit =
    if currentBit < 0 then tmp_lst
    else 
      (* comparaison pour ajouter soit true soit false *)
      let result_bool = (Int64.logand n (Int64.shift_left 1L currentBit) <> 0L) in
      aux (result_bool :: tmp_lst) (currentBit - 1)
  in
  aux [] 63;;

(* Q1.2 |
  description : chercher l'index du dernier true dans la 'liste binaire'
  parametre : liste a chercher
  return : entier index
*)
let find_last_true_index lst =
  let rec aux l count last_position =
    match l with
    | [] -> last_position
    | h::t -> 
        let new_position = if h then count else last_position in
        aux t (count + 1) new_position
  in
  aux lst 0 (-1)
;;

(* Q1.2 |
  description : pour le dernier element enlever les false inutile (00000)100
  parametre : liste a trimmer
  return : la liste boolean sans les false inutile
*)
let trim_list lst =
  let last_true_pos = find_last_true_index lst in
  if last_true_pos = -1 then [](* pas de true *)
  else 
    (* on connait l'index : cree une fonction qui permet d'accumuler des bools dans un liste tant qu'on est pas au dernier true *)
    let f tmp_lst hd = 
      if (List.length tmp_lst) <= last_true_pos then hd::tmp_lst 
      else tmp_lst (* rien rajouter *)
    in
    List.rev (List.fold_left f [] lst)
;;


(* Q1.2 | 
  description : decomposition de la liste entier64 en une liste boolean,
  parametre 'list' : liste de int64
  return : liste boolean
*)
let decomposition (lst:int64 list) =
  let rec aux l tmp_lst =
    (* on prend le binaire de chaque element sous forme de liste boolean *)
    let binaire = decomposition_aux (List.hd l) in

    match l with 
    |[] -> []
    (* special case pour le dernier element *)
    |[_] -> tmp_lst @ (trim_list binaire)
    |tete::reste -> aux reste (tmp_lst @ binaire)
  in
  aux lst []
;;



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

<<<<<<< HEAD
Printf.printf "%s\n\n" exff_result;;

Printf.printf "%s\n\n" "Arbre : ";;

(* Ex 2.7 *)
type arbre =
  | Noeud of int * arbre * arbre
  | Feuille of bool

let rec take l n =    (*取出前n个值*)
  match l with
  | [] -> []
  | h::t -> if n = 0 then [] else h :: take t (n-1)


let rec drop l n =  (*扔掉前n个值*)
  match l with
  | [] -> []
  | h::t -> if n = 0 then l else drop t (n-1)

let rec cons_arbre liste =
  let rec aux l depth =
    let n = List.length l in
    if n = 1 then 
      Feuille (List.hd l)
    else
      let mid = n / 2 in
      let left_list = take l mid in
      let right_list = drop l mid in
      Noeud(depth, aux left_list (depth+1), aux right_list (depth+1))
  in
  let next_power_of_two = 
    int_of_float (2.0 ** (ceil (log (float_of_int (List.length liste)) /. log 2.0))) in
  let full_list = completion liste next_power_of_two in
  aux full_list 1
  

let liste = [true; true; false; true; false; false; true; false; true; false]
let tree = cons_arbre liste

let rec print_arbre = function
  | Feuille b -> Printf.printf "Feuille(%B) " b
  | Noeud(n, left, right) -> 
      Printf.printf "Noeud(%d) (" n;
      print_arbre left;
      Printf.printf ") (";
      print_arbre right;
      Printf.printf ")";;

let rec print_arbre_with_indent tree indent =
  match tree with
  | Feuille b -> Printf.printf "%sFeuille(%B)\n" indent b
  | Noeud(n, left, right) ->
      Printf.printf "%sNoeud(%d)\n" indent n;
      print_arbre_with_indent left (indent ^ "  ");
      print_arbre_with_indent right (indent ^ "  ")

let print_arbre tree = print_arbre_with_indent tree "";;
      



print_arbre tree;

Printf.printf "\n";

let rec liste_feuilles tree =
  match tree with
  | Feuille b -> [b]
  | Noeud(_, left, right) -> (liste_feuilles left) @ (liste_feuilles right);;

  let new_list = liste_feuilles tree;;
  

=======
Printf.printf "%s\n\n" exff_result;;
>>>>>>> 6ab8b016779eec9a967344d8591bac7739c9634c
