(* importation *)
open Int64;;

(* Question 1.1 
  - entier de grand precision
  - definir des listes d'entiers 64 bits
  - insertion en fin de liste    
  - preparer structure de donnees 
  - primitive insertion
  - primitive recuperation
  - primitive suppression (tete de liste)
  2^100 depasse 64bits
  100 mod 64 = 36 (reste)
  donc la liste [0;2^36]

  //pas bon
  si 2^164 depasse 64bits
  164/64 ~ 2
  donc 164 - 64 - 64 = 36
  on a [0;0;36]

  explication 2^100 = [0;2^36] ou [OL; Int64.shift_left 1L 36]
  1L pour 1 = 2^0 
  si on decale de 36 a gauche de 2^0 on a 2^0 * 2^36 = 2^36 
  donc en binaire : 00000...1000000 a la position 
*)

(* creation de struct de donnees, suffixe L*)
type bigInteger = int64 list;;

let rec prim_insertion_aux n l =
  if n>63 then 
    let minus = n - 64 in 
    prim_insertion_aux minus (l @ [0])
else 
  l @ [Int64.shift_left 1L n];;

(* n est la puissance de 2 : ex, 2^100 -> n=100 *)
let prim_insertion n =  
  prim_insertion_aux n [];;
