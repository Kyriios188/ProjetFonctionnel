Printf.printf "//////////////////////////////////////////////////%s%!" "ici";;
type id = int

type 'a out_arcs = (id * 'a) list

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a out_arcs) list
let clone_nodes (gr:'a graph) = List.map (fun (id, _) -> (id, [])) gr;;
let test = [(1, [(2, "1v2"); (3, "1v3")]); (3, [(4, "3v4")]); (4, [])];;

clone_nodes test
