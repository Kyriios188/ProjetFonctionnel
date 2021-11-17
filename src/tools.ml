(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
(* let rec clone_nodes_map (gr:'a graph) = List.map (fun (id, arcs) -> id) gr
   pourquoi ça marche pas ??*)



let clone_nodes (gr:'a graph) = n_fold gr (fun acu id -> new_node acu id) empty_graph;;

let gmap (gr:'a graph) f = e_fold gr (fun acu id1 id2 label -> new_arc acu id1 id2 (f label)) (clone_nodes gr);;

let add_arc (gr:'a graph) id1 id2 value = match (find_arc gr id1 id2) with
  | None -> new_arc gr id1 id2 value
  | Some l -> new_arc gr id1 id2 (l+value);;

(*
let ford_fulkerson gr n1 n2 acu = match (exist gr) with
  | Some -> ford_fulkerson gr n1 n2
  | None -> flot_max gr n1 n2

*)