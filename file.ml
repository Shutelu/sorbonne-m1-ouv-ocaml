(* scan element n pour trouver les bit de 1 et retourne liste de bool*)
let decomposition_aux n =
  let rec aux acc_bool_list currentBit =
    if currentBit < 0 then acc_bool_list
    else 
      let resbool = (Int64.logand n (Int64.shift_left 1L currentBit) <> 0L) in
      aux (resbool :: acc_bool_list) (currentBit - 1)
  in
  aux [] 63;;

(** 找到最后一个 true 在 bool 列表中的索引 *)
let rec find_last_true_index lst idx last_true =
  match lst with
  | [] -> last_true
  | h::t -> 
      let new_last = if h then idx else last_true in
      find_last_true_index t (idx + 1) new_last
;;

(** 裁剪列表，仅包括最后一个 true 之前的所有项（包括它） *)
let trim_list lst =
  let last_true_pos = find_last_true_index lst 0 (-1) in
  if last_true_pos = -1 then []
  else 
    let f acc hd = if (List.length acc) <= last_true_pos then hd::acc else acc in
    List.rev (List.fold_left f [] lst)
;;


(* Q1.2 | 
  description : decomposition de la liste entier64 en une liste boolean
  parametre 'list' : liste de int64
  return : liste boolean
*)
let decomposition (list: int64 list) =
  let taille = List.length list in

  let process_single_int64 i n =
    let bits = decomposition_aux n in
    if i < taille - 1 then bits else trim_list bits
  in

  let result_list = List.mapi process_single_int64 list in
  List.flatten result_list
;;

let decomposition_test list =
  let rec aux l temp_list =
    let dec_list = decomposition_aux (List.hd l) in
    match l with 
    |[] -> []
    |[hd] -> temp_list @ (trim_list dec_list)
    |tete::reste -> aux reste (temp_list @ dec_list)
  in
  aux list []
;;


(*======================== 
          TEST 
=========================*)
(*prend un entier64 (liste) *)

let b = [0L; Int64.shift_left 1L 36];; 
let a = [5L];; 
let result = decomposition_test [55L] ;;
(*verification de la liste pour le dev*)
List.iter (fun b -> Printf.printf "%b " b) result;