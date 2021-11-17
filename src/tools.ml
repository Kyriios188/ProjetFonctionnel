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
let test = new_node empty_graph 0;;
let t2 = new_node test 1;;
let t3 = new_node t2 2;;
let t4 = new_node t3 3;; (* t4 contient 4 nodes de 0 à 3 *)
let t5 = new_arc t4 1 2 "de1vers2";;
let t6 = new_arc t5 1 3 "de1vers3";;
*)


