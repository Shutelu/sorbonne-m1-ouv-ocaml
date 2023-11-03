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
