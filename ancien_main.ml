(* importation *)
open Int64;;

(* Question 1.1 
  2^100 depasse 64bits
  100 mod 64 = 36 (reste)
  donc la liste [0;2^36]

  explication 2^100 = [0;2^36] ou [OL; Int64.shift_left 1L 36]
  1L pour 1 = 2^0 
  si on decale de 36 a gauche de 2^0 on a 2^0 * 2^36 = 2^36 
  donc en binaire : 00000...1000000 a la position 
*)

(* Q1.1 | aux *)
let rec _prim_insertion_aux n l =
  if n>63 then 
    let minus = n - 64 in 
    prim_insertion_aux minus (l @ [0])
  else 
    l @ [Int64.shift_left 1L n]
;;

(* Q1.1 | 
  - creation du grand entier sous forme de liste 
  param n : la puissance de 2, ex, 2^100 -> n=100
*)
let prim_insertion n =  
  _prim_insertion_aux n []
;;

(* Q1.1 | Reccuperation de la tete de liste *)
let prim_reccup l = List.hd l;;
(* Q1.1 | Retourne la liste sans la tete de liste *)
let prim_supp l =  List.tl l;;

(* Q1.2 | fonction qui retourne une liste de false : pour la decomposition*)
let rec _getDecFalseTrueList countTo count list =
  if count = countTo then list
  else 
    _getDecFalseTrueList countTo (count+1) (false :: list)
;;

(* Q1.2 | fonction qui retourne la puissance de 2 : pour la decomposition*)
let rec _pOf2 count bigInt = 
  if Int64.equal 1L bigInt then count
  else _pOf2 (count + 1) (Int64.shift_right_logical bigInt 1)
;;
  
let rec decomposition_aux l  listeTemp =
  match l with 
  |[] -> listeTemp @ [true]
  |t::q -> 
      if Int64.equal t 0L then decomposition_aux q ((_getDecFalseTrueList 64 0 []) @ listeTemp) 
      else 
        let puiss_nb = _pOf2 0 t in
        decomposition_aux q ((_getDecFalseTrueList puiss_nb 0 []) @ listeTemp)
;;

(* Q1.2 | decomposition de la liste en liste boolean*)
let decomposition l = decomposition_aux l [];;

let dec = decomposition (prim_insertion_aux 65 []);;
