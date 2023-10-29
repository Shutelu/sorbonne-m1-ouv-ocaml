open Int64;;

(* Q1.1 *)
type bigInteger = int64 list;;
let prim_insertion lst e = lst @ [e];;
let prim_reccup lst = List.hd lst;;
let prim_supp lst = List.tl lst;;

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

(* Q1.3 | 
  description : extension de la liste avec des false
  parametre : count entier nombre d'ajout
  parametre : lst liste boolean 
  return : liste boolean avec des false (bits) en plus
*)
let rec extend_bool_lst count lst =
  if count > 0 then 
    (*ajout du false a la fin de la liste*)
    extend_bool_lst (count-1) (lst @ [false]) 
  else lst
;;

(* Q1.3 | 
  description : couper la liste de count fois
  parametre : count entier nombre de coupe
  parametre : lst liste boolean 
  return : liste boolean avec des bits en moins
*)
let rec cut_bool_lst count lst tmp_lst = 
  match lst with 
  |[] -> []
  |t::q -> 
    if count > 0 then cut_bool_lst (count-1) lst (tmp_lst @ [t])
    else tmp_lst
;;

(* Q1.3 | 
  description : renvoie une liste de n element
    tronquée jusqu'a n premiers element ou completé jusqu'a n avec des false
  parametre : bool_lst liste boolean a tronquée ou a completé
  parametre : n entier
  return : liste boolean tronquée ou completé de taille n
*)
let completion bool_lst n =
  let taille = List.length bool_lst in
  if n > taille then 
    extend_bool_lst (n-taille) bool_lst
  else if n < taille then
    cut_bool_lst n bool_lst []
  else 
    bool_lst
;;

(* ******************************************************************************************** *)

(* Q1.4 |
  description : transforme une liste boolean en entier64
  param : bool_lst liste boolean
  return : entier64 representer dans la liste
*)
let bool_list_to_int64 bool_lst =
  let reversed_lst = List.rev bool_lst in (* mettre a l'endroit *)
  (* le 1er 1 lance la procedure de decalage ensuite si true on fait +1 sinon +0 dans tout les cas on decale donc 2^n *)
  List.fold_left (fun acc b -> Int64.(add (shift_left acc 1) (if b then 1L else 0L))) 0L reversed_lst;;

(* Q1.4 |
  description : prend une liste de boolean, decoupe la liste jusqu'a obtenir une liste de 64 boolean et retourne la sous liste et le reste
  param : lst liste a decouper
  return : si taille de lst <64 la meme liste, sinon une liste de 64 element, et reste de lst
*)
let split_list_by64 lst =
  let rec aux l tmp_lst count =
    match l with
    |[]->(List.rev tmp_lst, l) (* soit taille lst < 64 *)
    |t::reste -> 
      if count = 0 then (List.rev tmp_lst, l) (* soit taille lst > 64 *)
      else aux reste (t::tmp_lst) (count-1)
  in
  aux lst [] 64
;;

(* Q1.4 |
  description : prend une liste de boolean et renvoie une liste de liste decouper si possible en taille 64 ex :[1;0;1] -> [[1];[0];[1]]
  param : lst liste boolean
  return : une liste de sous liste boolean
*)
let rec split_list_to_sublists lst =
  if lst = [] then []
  else 
    let (sous_lst, reste_lst) = split_list_by64 lst in
    sous_lst :: split_list_to_sublists reste_lst
;;

(* Q1.4 | 
  description : prend une liste de bits (bool) et contruction l'entier qui le represente 
  param : une liste de boolean qui represente un nb binaire
  return : entier que le binaire represente sous forme de int64 list
*)
let composition bool_lst : int64 list =
  let list_of_boollist = split_list_to_sublists bool_lst in
  List.map bool_list_to_int64 list_of_boollist
;;

(* TEST *)

let b = [0L; Int64.shift_left 1L 36];;
let a = [38L];;
let dec = decomposition b;;
(*verification de la liste pour le dev*)
List.iter (fun b -> Printf.printf "%b " b) dec;;
Printf.printf "\n" ;;

let composition_result = composition dec;;
List.iter (fun i -> Printf.printf "%Ld " i) composition_result;;
Printf.printf "\n" ;;

(* affichage pour le dev *)
let print_int64_list list =
  let string_of_int64_elems elems =
    let strs = List.map (fun i -> Printf.sprintf "%Ld" i) elems in
    String.concat "; " strs
  in
  Printf.printf "on obtient : [%s]\n" (string_of_int64_elems list);;

print_int64_list composition_result;;

