(* Importation *)
open Int64;;
open Random;;

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
  aux [] 63
;;

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
    if count > 0 then cut_bool_lst (count-1) q (tmp_lst @ [t])
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

(* Q1.4 |
  description : transforme une liste boolean en entier64
  param : bool_lst liste boolean
  return : entier64 representer dans la liste
*)
let bool_list_to_int64 bool_lst =
  let reversed_lst = List.rev bool_lst in (* mettre a l'endroit *)
  (* le 1er 1 lance la procedure de decalage ensuite si true on fait +1 sinon +0 dans tout les cas on decale donc 2^n *)
  List.fold_left (fun acc b -> Int64.(add (shift_left acc 1) (if b then 1L else 0L))) 0L reversed_lst
;;

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

(* Q1.5 |
  description : table de verité, prendre un entier64 'x', le decompose en base 2, le complete pour faire la taille n
  param x : entier64 (int64 list)
  param n : taille qu'aura la table de verité
  return : liste boolean (la table de verité)
*)
let table x n = completion (decomposition x) n;;

(* Q1.6 | 
   description : genere aleatoirement un entier sur n bits sous forme de liste
   param n : le nombre max de bits de l'entier
   return : une liste de int64 qui represente notre entier aleatoire
*)
let genAlea n = 
  let rec aux tmp_lst count = 
    if count > 64 then 
      let tmp = tmp_lst @ [Random.int64 Int64.max_int] in
      aux tmp (n-64)
    else
      tmp_lst @ [Random.int64 (Int64.shift_left 1L n)]
  in
  Random.self_init (); 
  aux [] n
;;

(* Q2.7 *)
type arbre =
  | Noeud of arbre ref * int * arbre ref
  | Feuille of bool ref 
;;

(* ******************************************************************************************** *)

(* Q2.8 | 
  description : prendre une liste et garde les n premiers elements
  param l : la liste boolean 
  param n : le nombre de premier element
  return : la liste boolean gardant les n premiers elements
*)
let rec take l n =
  match l with
  | [] -> []
  | h::t -> if n = 0 then [] else h :: take t (n-1)

(* Q2.8 | 
  description : prendre une liste et jete les n premiers element, pour garder le reste
  param l : la liste boolean 
  param n : le nombre de premier element
  return : la liste boolean privee de ses n premier element
*)
let rec drop l n = 
  match l with
  | [] -> []
  | h::t -> if n = 0 then l else drop t (n-1)
;;

(* Q2.8 |
  description : prend une list boolean, si sa taille ne correspond pas a une puissance de 2,
  lui applique la fonction completion pour lui donner une taille de puissance de 2 (borne superieur) 
  param bool_lst : la liste boolean a reparer
  return : liste boolean avec taille reparer
*)
let repair_lst_power bool_lst =
  let taille = List.length bool_lst in
  let rec aux l t count =
    let taille64 = Int64.of_int taille in
    let power_of2 = Int64.shift_left 1L count in

    if taille64 = power_of2 || count = 64 then bool_lst
    else if taille64 < power_of2 then completion bool_lst (Int64.to_int power_of2) 
    else aux l t (count+1)
  in
  aux bool_lst taille 0
;;

(* Q2.8 |
   description : construit l'arbre de decision correspondant a la table de verite, l'arbre est equilibre
   param bool_lst : la table de verite
   return : la racine de notre arbre
*)
let cons_arbre bool_lst =
  let repaired_lst = repair_lst_power bool_lst in
  let rec aux l profondeur =
    let taille = List.length l in
    if taille = 1 then Feuille(ref (List.hd l))
    else 
      let mid = taille /2 in
      let lst_gauche = take l mid in
      let lst_droit = drop l mid in
      Noeud(ref(aux lst_gauche (profondeur+1)), profondeur , ref(aux lst_droit (profondeur+1)))
  in
  aux repaired_lst 1
;;

(* Q2.9 |
  description : contruit la liste des etiquettes des feuilles du sous arbre enracine en N
  param tree : le sous arbre
  return : les feuilles du sous arbre, en tant que liste boolean   
*)
let rec liste_feuilles arbre =
  match arbre with
  | Feuille b -> [!b]
  | Noeud(left, _, right) -> (liste_feuilles !left) @ (liste_feuilles !right)
;;

(* 3.10 *)
type listDejaVue = (int64 list * arbre) list




(* let liste_ldv = [(1,Feuille true);(2,Feuille false)];;
let boolean, lst = recherche_ldv 2 liste_ldv;;
let l1,l2 = List.hd lst;;  *)



(* 3.11  *)

(*3.11 recherche si element est dans ldv*)
let rec recherche_ldv element ldv =
  match ldv with
  |[] -> (false, ldv) (* ldv ici ne sert a rien*)
  |t::q -> 
      let nb, arb = t in
      if nb = element then (true, [(t)]) 
      else recherche_ldv element q
;;

(* regarde si la motier gauche de lst est false*)
let gauche_moitier_false lst =
  let dropped_lst = drop lst ((List.length lst)/2) in
  let rec aux l =
    match l with
    |[] -> true
    |t::q -> if t = true then false else aux q 
  in aux dropped_lst
;;

let rec compressionParListe (arbre: arbre ref) ldv = 
  match !arbre with
  | Feuille b -> 
    let grand_entier = composition (liste_feuilles !arbre) in
    let boolean_recherche, lst_tuple_recherche = recherche_ldv grand_entier ldv in

    (* pas dans ldv, ajout *)
    if boolean_recherche = false then 
      let gn , feuille = (grand_entier, Feuille b) in
      let ldv_retour = ldv @ [(gn, feuille)] in
      (feuille, ldv_retour)
    (* dans ldv, retour *)
    else
      let nb_ret_true, feuille = (List.hd lst_tuple_recherche) in
      (feuille, ldv)

  | Noeud(filsGauche, prof , filsDroite) ->
    let transformed_node = liste_feuilles !arbre in
    let gauche_false = gauche_moitier_false transformed_node in

    (*pas trouver moitier gauche false*)
    if gauche_false = false then
      let grand_entier = composition transformed_node in
      let boolean_recherche, lst_tuple_recherche = recherche_ldv grand_entier ldv in

      (* pas dans ldv alors ajout*)
      if boolean_recherche = false then 
        (* fils gauche*)
        let node_fils_gauche,ldv_fils_gauche = compressionParListe filsGauche ldv in
        let node_fils_droite,ldv_fils_droite = compressionParListe filsDroite ldv_fils_gauche in
        filsGauche := node_fils_gauche;
        filsDroite := node_fils_droite;
        let newLDV = ldv_fils_droite @ [(grand_entier, Noeud(filsGauche, prof, filsDroite))] in
        (Noeud(ref node_fils_gauche,prof, ref node_fils_droite), newLDV)
      (*dans ldv alors renvoie*)
      else
        let nb_ret_true, node_ret_true = (List.hd lst_tuple_recherche) in
        (node_ret_true, ldv)

    (*trouver moitier gauche false*)
    else
      let node_fils_gauche,ldv_fils_gauche = compressionParListe filsGauche ldv in
      (node_fils_gauche, ldv)
;;

    
let dec = decomposition [25899L];;
let tree = cons_arbre dec;;


let rec print_arbre = function
  | Feuille b -> Printf.printf "Feuille(%B) " !b
  | Noeud(left, n, right) -> 
      Printf.printf "Noeud(%d) (" n;
      print_arbre !left;
      Printf.printf ") (";
      print_arbre !right;
      Printf.printf ")";;

let rec print_arbre_with_indent tree indent =
  match tree with
  | Feuille b -> Printf.printf "%sFeuille(%B)\n" indent !b
  | Noeud(left, n, right) ->
      Printf.printf "%sNoeud(%d)\n" indent n;
      print_arbre_with_indent !left (indent ^ "  ");
      print_arbre_with_indent !right (indent ^ "  ")

let print_arbre tree = print_arbre_with_indent tree "";;
print_arbre tree;;

let result = compressionParListe (ref tree) [];;
let node, ldv = result;;

print_arbre tree;;
Printf.printf "\n";;


let to_dot arbre =
  let buffer = Buffer.create 1024 in
  let node_id = ref 0 in
  let node_map = Hashtbl.create 100 in

  let get_id a = 
    try 
      Some (Hashtbl.find node_map a)
    with Not_found -> None
  in

  let add_id a id = 
    Hashtbl.add node_map a id 
  in

  let rec helper a =
    match get_id a with
    | Some id -> id  (* Return the existing id *)
    | None -> (
      match a with
      | Feuille(b) -> 
        let id = !node_id in
        incr node_id;
        add_id a id;
        Buffer.add_string buffer (Printf.sprintf "%d [label=\"%b\"];\n" id !b);
        id
      | Noeud(l, v, r) ->
        let id = !node_id in
        incr node_id;
        add_id a id;
        Buffer.add_string buffer (Printf.sprintf "%d [label=\"%d\"];\n" id v);
        let left_id = helper !l in
        let right_id = helper !r in
        Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=dotted];\n" id left_id);
        Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=solid];\n" id right_id);
        id
    )
  in

  Buffer.add_string buffer "graph G {\n";
  ignore (helper arbre);
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer



let save_to_dot_file filename arbre =
  let dot_content = to_dot arbre in
  let oc = open_out filename in
  output_string oc dot_content;
  close_out oc

let my_tree = tree;;
save_to_dot_file "my_tree.dot" my_tree



let rec print_node n =
  match n with
  | Feuille(b) -> Printf.printf "%b" !b
  | Noeud(l, v, r) -> 
      Printf.printf "("; 
      print_node !l; 
      Printf.printf ", %d, " v; 
      print_node !r;
      Printf.printf ")"

let print_int64_list lst =
  List.iter (fun i -> Printf.printf "%Ld " i) lst

let print_listDejaVu_element (lst, n) =
  print_int64_list lst;
  Printf.printf " : ";
  print_node n

let print_listDejaVu l =
  let rec aux = function
      | [] -> ()
      | [x] -> print_listDejaVu_element x
      | x :: xs -> 
          print_listDejaVu_element x; 
          Printf.printf " || "; 
          aux xs
  in
  aux l;
  print_newline ();;

  print_listDejaVu ldv;;



let to_dot_ldv ldv =
  let buffer = Buffer.create 1024 in
  let node_id = ref 0 in
  let node_map = Hashtbl.create 100 in

  let get_id a = 
    try 
      Some (Hashtbl.find node_map a)
    with Not_found -> None
  in

  let add_id a id = 
    Hashtbl.add node_map a id 
  in

  let rec helper (lst, a) =
    match get_id a with
    | Some id -> id  (* Return the existing id *)
    | None -> (
      match a with
      | Feuille(b) -> 
        let id = !node_id in
        incr node_id;
        add_id a id;
        Buffer.add_string buffer (Printf.sprintf "%d [label=\"%b\"];\n" id !b);
        id
      | Noeud(l, v, r) ->
        let id = !node_id in
        incr node_id;
        add_id a id;
        Buffer.add_string buffer (Printf.sprintf "%d [label=\"%d\"];\n" id v);
        let left_id = helper (lst, !l) in
        let right_id = helper (lst, !r) in
        Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=dotted];\n" id left_id);
        Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=solid];\n" id right_id);
        id
    )
  in

  Buffer.add_string buffer "graph G {\n";
  List.iter (fun tuple -> ignore (helper tuple)) ldv;
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer
  

let save_to_dot_file_ldv filename ldv =
  let dot_content = to_dot_ldv ldv in
  let oc = open_out filename in
  output_string oc dot_content;
  close_out oc
  
  
  
let my_ldv = ldv;;
save_to_dot_file_ldv "my_ldv.dot" my_ldv;;