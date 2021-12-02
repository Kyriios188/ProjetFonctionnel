(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)

let clone_nodes (gr:'a graph) = n_fold gr (fun acu id -> new_node acu id) empty_graph;;

let gmap (gr:'a graph) f = e_fold gr (fun acu id1 id2 label -> new_arc acu id1 id2 (f label)) (clone_nodes gr);;

let add_arc (gr:int graph) id1 id2 value = match (find_arc gr id1 id2) with
  | None -> Printf.printf "J'ai pas trouvé d'arc %d->%d%!" id1 id2; Printf.printf " Je met la valeur %d\n%!" value; new_arc gr id1 id2 value
  | Some l -> Printf.printf "%d->%d passe de %d à %d\n%!" id1 id2 l (l+value); new_arc gr id1 id2 (l+value);;