(* Importation *)
open Int64;;
open Random;;

(* debut Q1.1 *********************************************************************************************************)
(* Q1.1 | description : definition de la structure des entiers avec grande précision *)
type bigInteger = int64 list;;

(* Q1.1 | 
  description : ajout de l'element 'e' en fin de la liste 'lst'
  param lst : liste où on ajoute 'e'
  param e : element a ajouter
  return : la liste avec 'e' ajouter en fin de liste
*)
let primitive_insertion lst e = lst @ [e];;

(* Q1.1 | 
  description : reccupere la tete de la liste
  param lst : liste où on reccupere la tete
  return : la tete de la liste
*)
let primitive_reccuperation lst = List.hd lst;;

(* Q1.1 | 
  description : renvoie la liste sans la tete de liste
  param lst : liste où on renvoie tout sauf la tete
  return : le reste de la liste sans la queue
*)
let primitive_suppression lst = List.tl lst;;
(* fin Q1.1 *********************************************************************************************************)


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
let decomposition (lst:bigInteger) =
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







(*****************************************************************************************************************)

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


(*************************************************************************************************************************)

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
  return : entier que le binaire represente sous forme de bigInteger
*)
let composition bool_lst : bigInteger =
  let list_of_boollist = split_list_to_sublists bool_lst in
  List.map bool_list_to_int64 list_of_boollist
;;





(********************************************************************************************************************)
(* Q1.5 |
  description : table de verité, prendre un entier64 'x', le decompose en base 2, le complete pour faire la taille n
  param x : entier64 (bigInteger)
  param n : taille qu'aura la table de verité
  return : liste boolean (la table de verité)
*)
let table x n = completion (decomposition x) n;;


(*********************************************************************************************************************)
(* Q1.6 | 
   description : genere aleatoirement un entier sur n bits sous forme de liste
   param n : le nombre max de bits de l'entier
   return : une liste de int64 qui represente notre entier aleatoire
*)
let genAlea n = 
  let rec aux acc n = 
    if n <= 0 then acc
    else
      let bits_to_generate = if n > 64 then 64 else n in  (* Determine bits to generate *)
      let new_rand = if bits_to_generate = 64 then
                       Random.int64 Int64.max_int  (* Generate a full 64-bit random number *)
                     else
                       let max_value_for_bits = Int64.sub (Int64.shift_left 1L bits_to_generate) 1L in
                       Random.int64 max_value_for_bits  (* Generate a random number for the remaining bits *)
      in
      aux (new_rand :: acc) (n - bits_to_generate)  (* Recurse for the remaining bits *)
  in
  Random.self_init (); 
  aux [] n
;;





(* Q2.7 *)
type arbre =
  | Noeud of arbre ref * int * arbre ref
  | Feuille of bool ref 
;;

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
;;

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

(**********************************************************************************************************************)
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
(* Test *)


(**********************************************************************************************************************)
(* 3.10 *)
type listDejaVue = (bigInteger * arbre) list;;

(* Q3.11 |
  description : recherche si element est dans la premiere composante de ldv
  param element : element a rechercher
  param ldv : liste utiliser pour la recherche
  return : couple de , boolean indiquant la presence de element, la liste ldv ou element trouver 
*)
let rec recherche_ldv element ldv =
  match ldv with
  |[] -> (false, ldv) (* ldv ici ne sert a rien*)
  |t::q -> 
      let nb, arb = t in
      if nb = element then (true, [(t)]) 
      else recherche_ldv element q
;;

(* Q3.11 | 
  description : regarde si la motier droite de lst est false
  param lst : la liste a verifier
  return : true si moitier droite est contient entierement des false, sinon false
*)
let gauche_moitier_false lst =
  let dropped_lst = drop lst ((List.length lst)/2) in
  let rec aux l =
    match l with
    |[] -> true
    |t::q -> if t = true then false else aux q 
  in aux dropped_lst
;;

(* Q3.11 |
  description : compressions de l'arbre fourni en utilisant listDejaVue
  param arbre : l'arbre a compresser
  param ldf : listeDejaVue , une liste vide au depart
  return : une couple, l'arbre compresser et ldv remplie
*)
let rec compressionParListe (arbre: arbre) ldv = 
  match arbre with
  | Feuille b -> 
    let grand_entier = composition (liste_feuilles arbre) in
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
    let transformed_node = liste_feuilles arbre in
    let gauche_false = gauche_moitier_false transformed_node in

    (*pas trouver moitier gauche false*)
    if gauche_false = false then
      let grand_entier = composition transformed_node in
      let boolean_recherche, lst_tuple_recherche = recherche_ldv grand_entier ldv in

      (* pas dans ldv alors ajout*)
      if boolean_recherche = false then 
        (* fils gauche*)
        let node_fils_gauche,ldv_fils_gauche = compressionParListe !filsGauche ldv in
        let node_fils_droite,ldv_fils_droite = compressionParListe !filsDroite ldv_fils_gauche in
        let newLDV = ldv_fils_droite @ [(grand_entier, Noeud(ref node_fils_gauche, prof, ref node_fils_droite))] in
        (Noeud(ref node_fils_gauche,prof, ref node_fils_droite), newLDV)
      (*dans ldv alors renvoie*)
      else
        let nb_ret_true, node_ret_true = (List.hd lst_tuple_recherche) in
        (node_ret_true, ldv)

    (*trouver moitier gauche false*)
    else
      let node_fils_gauche,ldv_fils_gauche = compressionParListe !filsGauche ldv in
      (node_fils_gauche, ldv)
;;

(********************************************************************************************************************)

(* 3.13 AND Test *)

let to_dot arbre =
  let buffer = Buffer.create 1024 in
  let node_id = ref 0 in

  let rec helper a =
    let id = !node_id in
    incr node_id;
    match a with
    | Feuille(b) -> 
      Buffer.add_string buffer (Printf.sprintf "%d [label=\"%b\"];\n" id !b);
      id
    | Noeud(l, v, r) ->
      Buffer.add_string buffer (Printf.sprintf "%d [label=\"%d\"];\n" id v);
      let left_id = helper !l in
      let right_id = helper !r in
      Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=dotted];\n" id left_id);
      Buffer.add_string buffer (Printf.sprintf "%d -- %d [style=solid];\n" id right_id);
      id
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


  (* 3.14 AND Test *)

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
;;

(***********************************************************)

(* 4.15 *)
type arbreDejaVu = arbre

(* 4.16 *)
let compressionParArbre (arbre: arbre ref) = 
  let rec aux arbre_ref =
    match !arbre_ref with
    | Feuille b -> 
      let grand_entier = composition (liste_feuilles !arbre_ref) in
      let boolean_recherche, _ = recherche_ldv grand_entier [] in

      if boolean_recherche = false then 
        Feuille b
      else
        Feuille b

    | Noeud(filsGauche, prof , filsDroite) ->
      let transformed_node = liste_feuilles !arbre_ref in
      let gauche_false = gauche_moitier_false transformed_node in

      if gauche_false = false then
        let grand_entier = composition transformed_node in
        let boolean_recherche, _ = recherche_ldv grand_entier [] in

        if boolean_recherche = false then 
          let node_fils_gauche = aux filsGauche in
          let node_fils_droite = aux filsDroite in
          Noeud(ref node_fils_gauche, prof, ref node_fils_droite)
        else
          Noeud(filsGauche, prof, filsDroite)
      else
        aux filsGauche
  in

  let result = aux arbre in
  arbre := result;
  arbre;;




(* 4.18 AND Test *)
let to_dot_c arbre =
  let buffer = Buffer.create 1024 in
  let node_id = ref 0 in
  let node_map = Hashtbl.create 100 in

  let get_id_c a = 
    try 
      Some (Hashtbl.find node_map a)
    with Not_found -> None
  in

  let add_id_c a id = 
    Hashtbl.add node_map a id 
  in

  let rec helper a =
    match get_id_c a with
    | Some id -> id  (* Return the existing id *)
    | None -> (
      match a with
      | Feuille(b) -> 
        let id = !node_id in
        incr node_id;
        add_id_c a id;
        Buffer.add_string buffer (Printf.sprintf "%d [label=\"%b\"];\n" id !b);
        id
      | Noeud(l, v, r) ->
        let id = !node_id in
        incr node_id;
        add_id_c a id;
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
;;

let save_to_dot_file_c filename arbre =
  let dot_content = to_dot_c arbre in
  let oc = open_out filename in
  output_string oc dot_content;
  close_out oc;;




(******************************************************)
(* 5.19 *)

(*
CompressionParListe : 

Complexité Temporelle :
Pour chaque nœud, l'algorithme calcule un grand_entier, ce qui nécessite de parcourir l'ensemble de l'arbre, donc la complexité est de O(n), où n est le nombre de nœuds dans l'arbre.
La fonction recherche_ldv, dans le pire des cas, doit parcourir la liste ldv entière. Si la longueur de ldv est m, alors la complexité de cette fonction est O(m).
Pour chaque nœud, l'algorithme peut avoir besoin d'appeler récursivement lui-même deux fois (une fois pour le sous-arbre gauche et une fois pour le sous-arbre droit).
Ainsi, la complexité temporelle totale de l'algorithme est de O(n^2 * m). Cela est dû au fait que pour chaque nœud, nous pourrions avoir à parcourir l'ensemble de l'arbre et à rechercher dans toute la liste ldv.

Complexité Memoire :
La principale consommation d'espace provient de la liste ldv et de la table de hachage node_map. Dans le pire des cas, la taille de ces deux peut atteindre n.
Par conséquent, la complexité spatiale est de O(n).

CompressionParArbre:

Complexité Temporelle :
Similaire à compressionParListe, pour chaque nœud, l'algorithme calcule un grand_entier, ce qui nécessite de parcourir l'ensemble de l'arbre, donc la complexité est de O(n).
La complexité de la fonction recherche_ldv est de O(1), car elle est toujours appelée sur une liste vide.
Pour chaque nœud, l'algorithme peut avoir besoin d'appeler récursivement lui-même deux fois.
Ainsi, la complexité temporelle totale de l'algorithme est de O(n^2).

Complexité Memoire :
Cet algorithme n'utilise pas la liste ldv, donc la principale consommation d'espace provient de la pile d'appels récursifs.
Par conséquent, la complexité spatiale est de O(n).

Conclusion :

La complexité temporelle de compressionParListe est de O(n^2 * m) et la complexité spatiale est de O(n).
La complexité temporelle de compressionParArbre est de O(n^2) et la complexité spatiale est de O(n).
*)

(*************************************************************************)

(* 6.20 *)
let rec count_noeuds (a: arbre) : int =
  match a with
  | Feuille _ -> 0  (* 如果是叶子节点，不增加计数 *)
  | Noeud (left, _, right) ->
      1 + (count_noeuds !left) + (count_noeuds !right)
      (* 如果是节点，计数增加1，并递归计算左右子树的节点数 *)
;;

(* taux compression AND complexite memoire*)
let lst_nombre_Noeud_avant = ref [];; (* 用来存原来的arbre的大小 *)
let lst_nombre_Noeud_apres = ref [];; (* 用来存压缩后的arbre的大小 *)
let lst_taux_compression = ref [];;
let lst_Noeud_Bit = ref [];;

let start_6_20 = 1;;
let end_6_20 = 1500;;

for i = start_6_20 to end_6_20 do
  let list_1 = genAlea i in  (* 生成随机列表 *)
  let list_2 = decomposition list_1 in
  let arbre_1_ref = ref (cons_arbre list_2) in  (* 生成树 *)
  let noeuds_avant = count_noeuds !arbre_1_ref in  (* 计算原始树的节点数 *)
  lst_nombre_Noeud_avant := noeuds_avant :: !lst_nombre_Noeud_avant;  (* 保存原始节点数 *)

  ignore (compressionParArbre arbre_1_ref);  (* 压缩树 *)

  let noeuds_apres = count_noeuds !arbre_1_ref in  (* 计算压缩后的树的节点数 *)
  lst_nombre_Noeud_apres := noeuds_apres :: !lst_nombre_Noeud_apres;  (* 保存压缩后的节点数 *)

  (* 计算并保存压缩率，注意避免除以0的情况 *)
  let taux =  1. -. (float_of_int noeuds_apres) /. (float_of_int noeuds_avant) in
  lst_taux_compression := taux :: !lst_taux_compression;  (* 保存压缩率 *)
done;;


(* 保存压缩率列表到CSV文件 *)
let save_taux_to_csv filename lst_ref =
  let oc = open_out filename in
  Printf.fprintf oc "Index,TauxCompression\n"; (* 写入头部 *)
  List.iteri (fun i taux ->  (* 使用iteri来同时获取索引和压缩率 *)
    Printf.fprintf oc "%d,%f\n" i taux
  ) (List.rev !lst_ref);  (* 反转列表以得到正确的顺序 *)
  close_out oc;;

let save_apres_list_to_csv file_name list_ref =
  let oc = open_out file_name in
  List.iter (fun value -> Printf.fprintf oc "%d\n" value) (List.rev !list_ref);
  close_out oc;;
  



save_taux_to_csv "taux_compression.csv" lst_taux_compression;;
save_apres_list_to_csv "nombre_noeuds_apres.csv" lst_nombre_Noeud_apres;;



(* 
压缩率和2的次方有关 
当arbre的Noeuds的数量是2^n的时候，也就是说一个grand entier的bits数量是2^n
这个grandInteger的最大数位的右边没有很多的false
(根据cons_arbre得出，如果一个grandInteger的bits数正好是2^n，那么就不需要进行false的补位)
这个时候对arbre的压缩效率是最低的
反过来，如果它的bits数量刚好是2^n+1，那么就意味着它的右边有有至少2^n个false，也就是说
它的压缩效率最低就有50%，而它本身的压缩效率再加上去，就会形成图里的周期
*)



(* 6.21 *)
let nombre_N = 512;;
let nombre_start = 1;;
let nombre_end = 1000;;
let lst_nombre_Noeud = ref [];;  (* 使用引用来存储列表 *)


for i = nombre_start to nombre_end do
  let grand_nombre = genAlea nombre_N in
  let list_grand_nombre = decomposition grand_nombre in
  let arbre_grand_nombre_ref = ref (cons_arbre list_grand_nombre) in
  let _ = compressionParArbre arbre_grand_nombre_ref in  (* 压缩Arbre *)
  let count = count_noeuds !arbre_grand_nombre_ref in  (* 计算Noeuds的数量 *)
  lst_nombre_Noeud := !lst_nombre_Noeud @ [count];  (* 仅添加Noeuds的数量到列表 *)
done;;

(* 将Noeuds的数量列表保存到CSV文件中 *)
(* 定义一个函数来将 int list 保存到 CSV 文件中 *)
let save_list_to_csv filename lst =
  let oc = open_out filename in
  Printf.fprintf oc "NombreDeNoeuds\n"; (* CSV头部 *)
  List.iter (fun count ->
    Printf.fprintf oc "%d\n" count
  ) lst;
  close_out oc;;

(* 调用函数保存CSV文件 *)
let csv_filename = "nombre_noeuds.csv";;
save_list_to_csv csv_filename !lst_nombre_Noeud;;

(* ================================================================== Fonction de affichage des tests*)
let rec print_int64_list lst =
  match lst with
  | [] -> ()
  | [x] -> print_string (Int64.to_string x)
  | x :: xs -> 
      print_string (Int64.to_string x);
      print_string "; ";
      print_int64_list xs
;;

let rec print_node n =
  match n with
  | Feuille(b) -> Printf.printf "%b" !b
  | Noeud(l, v, r) -> 
      Printf.printf "("; 
      print_node !l; 
      Printf.printf ", %d, " v; 
      print_node !r;
      Printf.printf ")"
;;

let print_listDejaVu_element (lst, n) =
  print_int64_list lst;
  Printf.printf " : ";
  print_node n
;;

let print_listDejaVu l =
  let rec aux = function
      | [] -> ()
      | [x] -> print_listDejaVu_element x
      | x :: xs -> 
          print_listDejaVu_element x; 
          Printf.printf "\n"; 
          aux xs
  in
  aux l;
  print_newline ()
;;

(* affichage pour le dev *)
let print_int64_list list =
  let string_of_int64_elems elems =
    let strs = List.map (fun i -> Printf.sprintf "%Ld" i) elems in
    String.concat "; " strs
  in
  Printf.printf "on obtient : [%s]\n" (string_of_int64_elems list)
;;

let rec print_arbre_with_indent tree indent =
  match tree with
  | Feuille b -> Printf.printf "%sFeuille(%B)\n" indent !b
  | Noeud(left, n, right) ->
      Printf.printf "%sNoeud(%d)\n" indent n;
      print_arbre_with_indent !left (indent ^ "  ");
      print_arbre_with_indent !right (indent ^ "  ")
;;

let print_arbre tree = print_arbre_with_indent tree "";;

(* 定义函数print_bool_list来打印bool类型的list *)
let rec print_bool_list = function
  | [] -> () (* 如果list为空，不打印任何东西 *)
  | [x] -> print_endline (string_of_bool x) (* 如果是list的最后一个元素，打印后换行 *)
  | x :: xs -> 
      print_string (string_of_bool x); (* 打印当前元素 *)
      print_string "; "; (* 打印元素之间的分隔符 *)
      print_bool_list xs (* 递归地打印剩余的list *)
;;


(**************************************************************************************)
(*======================================= TEST =======================================*)
(**************************************************************************************)

(****************************)
(* debut TEST decomposition *)
(****************************)
let twoPow100 = [0L; Int64.shift_left 1L 36];; (*[0; 2^36]*)
let trente_huit = [38L];; 
let result_decomposition_twoPow100 = decomposition twoPow100 ;;
let result_decomposition_trente_huit = decomposition trente_huit ;;

Printf.printf "======================================= Affichage des resultats de decomposition: \n\n";;
Printf.printf "Resultat decomposition de 2^100 : \n";;
List.iter (fun b -> Printf.printf "%b " b) result_decomposition_twoPow100;;
Printf.printf "\n\n";;
Printf.printf "Resultat decomposition de 38 : \n";;
List.iter (fun b -> Printf.printf "%b " b) result_decomposition_trente_huit;;
Printf.printf "\n\n";;
(**************************)
(* fin TEST decomposition *)
(**************************)

(*************************)
(* debut TEST completion *)
(*************************)
Printf.printf "======================================= Affichage des resultats de completion: \n\n";;
let sujet_trente_huit = decomposition [38L];;
let comp_taille4 = completion sujet_trente_huit 4;;
let comp_taille8 = completion sujet_trente_huit 8;;

Printf.printf "Resultat completion pour une taille de 4 sur decomp de 38 :\n" ;;
List.iter (fun b -> Printf.printf "%b " b) comp_taille4;;
Printf.printf "\n\n";;
Printf.printf "Resultat completion pour une taille de 8 sur decomp de 38 :\n" ;;
List.iter (fun b -> Printf.printf "%b " b) comp_taille8;;
Printf.printf "\n\n";;
(***********************)
(* fin TEST completion *)
(***********************)
 
(**************************)
(* debut TEST composition *)
(**************************)
Printf.printf "======================================= Affichage des resultats de composition: \n\n";;
let dec_38 = decomposition [38L];;
let dec_2p100 = decomposition [0L; Int64.shift_left 1L 36];;
let composition_38 = composition dec_38;;
let composition_2p100 = composition dec_2p100;;

Printf.printf "Composition de 38 :\n";;
List.iter (fun b -> Printf.printf "%b " b) dec_38;;
Printf.printf "\n";;
print_int64_list composition_38;;
Printf.printf "\n" ;;

Printf.printf "Composition de 2^100 :\n";;
List.iter (fun b -> Printf.printf "%b " b) dec_2p100;;
Printf.printf "\n";;
print_int64_list composition_2p100;;
Printf.printf "\n" ;;

(************************)
(* fin TEST composition *)
(************************)

(********************)
(* debut TEST table *)
(********************)
Printf.printf "======================================= Affichage des resultats de table: \n\n";;
let table_taille4 = table [38L] 4;;
let table_taille11 = table [38L] 11;;

Printf.printf "table de 38 de taille 4 :\n";;
List.iter (fun i -> Printf.printf "%b " i) table_taille4;;
Printf.printf "\n\n" ;;
Printf.printf "table de 38 de taille 11 :\n";;
List.iter (fun i -> Printf.printf "%b " i) table_taille11;;
Printf.printf "\n\n" ;;
(******************)
(* fin TEST table *)
(******************)

(**********************)
(* debut TEST genAlea *)
(**********************)
Printf.printf "======================================= Affichage des resultats de genAlea: \n\n";;
let entier_aleatoire_100b = genAlea 100;;
let entier_aleatoire_512b = genAlea 512;;

Printf.printf "entier64 aleatoire sur 100 bits :\n";;
print_int64_list entier_aleatoire_100b;;
Printf.printf "\n" ;;
Printf.printf "entier64 aleatoire sur 512 bits :\n";; (* 512/64 = 8*)
print_int64_list entier_aleatoire_512b;;
Printf.printf "\n" ;;
(********************)
(* fin TEST genAlea *)
(********************)

(*************************)
(* debut TEST cons_arbre *)
(*************************)
Printf.printf "======================================= Affichage des resultats de cons_arbre: \n\n";;
let dec_25899 = decomposition [25899L];;
let tree_25899 = cons_arbre dec_25899;;

Printf.printf "exemple de construction d'arbre sur l'entier64 25899 :\n";;
print_arbre tree_25899;;
Printf.printf "\n";;
(***********************)
(* fin TEST cons_arbre *)
(***********************)

(*************************)
(* debut TEST cons_arbre *)
(*************************)
Printf.printf "======================================= Affichage des resultats de liste_feuilles: \n\n";;
let arbre_25899 = cons_arbre (decomposition [25899L]);;
let feuille_of_25899 = liste_feuilles arbre_25899;;

Printf.printf "Resultat des feuilles pour la racine 25899 :\n";;
print_bool_list feuille_of_25899;;
Printf.printf "\n";;
(***********************)
(* fin TEST cons_arbre *)
(***********************)

(**********************************)
(* debut TEST compressionParListe *)
(**********************************)
Printf.printf "======================================= Affichage des resultats de LDV: \n\n";;
let compressionParListe_arbre_25899 = cons_arbre (decomposition [25899L]);;
let root, ldv = compressionParListe compressionParListe_arbre_25899 [];;

Printf.printf "ListeDejaVu apres compressionParListe : \n";;
print_listDejaVu ldv;;
Printf.printf "\n";;
Printf.printf "L'arbre entier64 25899 apres compressionParListe : \n";;
print_arbre root;;
(********************************)
(* fin TEST compressionParListe *)
(********************************)

(**********************************)
(* debut TEST compressionParArbre *)
(**********************************)
Printf.printf "======================================= Affichage des resultats de compressionParArbre: \n\n";;
let tree_c = cons_arbre (decomposition [25899L]);;
let result_c = compressionParArbre (ref tree_c);;

Printf.printf "L'arbre entier64 25899 apres compressionParArbre : \n";;
print_arbre !result_c;;
(********************************)
(* fin TEST compressionParArbre *)
(********************************)

let my_tree = cons_arbre (decomposition [25899L]);;
save_to_dot_file "my_tree.dot" my_tree

let my_ldv = ldv;;
save_to_dot_file_ldv "my_ldv.dot" my_ldv;;

save_to_dot_file_c "my_arbre.dot" !result_c;;