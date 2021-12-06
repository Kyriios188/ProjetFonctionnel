open Graph
open Tools


type g_path = id list


type heap = id list
exception Empty_heap of string
exception Flot_optimal

(* fulkerson.ml *)
let stack (h:heap) (n:id) = n::h
let unstack (h:heap) = match h with
  | [] -> raise (Empty_heap "Cannot unstack empty heap")
  | x::t -> t
let heap_top (h:heap) = match h with
  | [] -> None
  | x::t -> Some x

let print_heap h = List.iter (Printf.printf "%d ") h; Printf.printf "\n%!"


(* 
On a une liste type [5; 4; 2; 0] qui retrace le chemin 0->2->4->5 
et on veut metre à jour les arcs correspondants avec le bon nombre
*)
(* heap est à l'envers *)
let update_work_graph ref_gr w_gr heap =
  (* h est à l'endroit quand on appelle la fonction *)
  let rec find_update_value (ref_gr:int graph) (w_gr: int graph) (h:heap) (acu:int) = match h with
    | [] | [_] -> acu (* Si ya 0 ou 1 éléments on a parcouru tous les arcs *)
    | x::(y::t) -> begin 
        match [(find_arc ref_gr x y); (find_arc w_gr x y)] with
        (* Si cet arc est plus restictif que les précédents, on baisse la valeur d'augmentation *)
        | [Some ref_label; Some work_label] -> (* Printf.printf "On peut augmenter de %d à %d\n%!" work_label ref_label; *)
          if ref_label - work_label < acu then find_update_value ref_gr w_gr t (ref_label-work_label) 
          else find_update_value ref_gr w_gr t acu

        | _ -> print_heap heap; failwith "Erreur lors de la recontruction du graph" (* Si ya un None c'est qu'on a pas trouvé l'arc dans un graph *)
      end

  in
  (* h est à l'endroit quand on appelle la fonction *)
  let rec update_graph acu_gr h update_value = match h with
    | [] | [_] -> acu_gr
    | x::(y::t) -> begin
        let un = add_arc acu_gr y x (-update_value) in
        update_graph (add_arc un x y update_value) (y::t) update_value
      end
  in

  let rev_heap = (List.rev heap) in
  (* On part du principe qu'il existe une arête de flot inférieur à 99999 dans le chemin parcouru *)
  let value = find_update_value ref_gr w_gr rev_heap 99999 in 

  Printf.printf "Valeur d'augmentation trouvée : %d\n%!" value;

  update_graph w_gr rev_heap value


(* Faut changer ça quand arc arrière *)
let find_capacite gr work_label begin_node next_node =

  match (find_arc gr begin_node next_node) with
  | None -> -work_label
  (* On vérifie si l'arc augmente la valeur et si la node d'arrivée n'est pas marquée *)
  | Some label_arc_ref -> label_arc_ref - work_label


(* Ne modifie pas le graph donné, donne juste un chemin augmentant *)
let find_augmenting_path (gr:int graph) (work_gr:int graph) (n_debut:id) (n_fin:id) = 

  (* Prend une liste d'arcs en entrée et doit trouver Some arc valide ou retourner None *)
  (* On lui donne la pile pour qu'il vérifie qu'il boucle pas *)
  let rec find_aug_arc (gr:int graph) (work_gr: int graph) arc_list begin_node visited_nodes h = match arc_list with
    | [] -> None

    (* On décompose l'arc sortant en node d'arrivée et label *)
    | (next_node, work_label)::t -> begin
        Printf.printf "On test la node d'arrivée %d\n%!" next_node;

        let capacite = find_capacite gr work_label begin_node next_node in

        if capacite >= 1 && not (List.mem next_node visited_nodes) && not (List.mem next_node h) then Some (next_node, work_label)
        (* L'arc n'est pas valide *)
        else find_aug_arc gr work_gr t begin_node visited_nodes h
      end

  in

  (*
  Conditions d'arrêt :
  _on atteint la node d'arrivée
  _on ne trouve pas d'arc augmentant partant d'une node qui est la node de début
  *)

  (* On créé une liste de nodes visitées, une pile et une valeur d'augmentation *)
  let rec find_aug_path gr work_gr n n_fin visited_nodes h = 

    (* On définit la méthode pour changer de node *)
    (* Lance find_aug_path pour la prochaine node dans la pile *)
    (* Au début de go_next, la node sans issue est dans la pile *)
    (* A la fin de go_next, la node sans issue n'est plus dans la pile *)
    let go_next gr work_gr n_fin visited_nodes h = 
      let next_node = (heap_top (unstack h)) in
      match next_node with
      | None -> raise Flot_optimal (* On a atteint la source et on a essayé de la dépiler i.e. la source est visitée *)
      (* Une node n'est visitée que si on a exploré tout ses chemins et qu'il n'y en a aucun de valide *)
      | Some x -> find_aug_path gr work_gr x n_fin visited_nodes (unstack h) 
    in

    (* Si on atteint la node d'arrivée on a trouvé un chemin augmentant *)
    (* Dans ce cas là même pas besoin de regarder les arcs sortants *)
    Printf.printf "On parcourt la node %d\n%!" n;
    if n==n_fin then h else

      (* On étudie les arcs sortants de la node actuelle n *)
      match (out_arcs work_gr n) with
      (* n est dans la pile, c'est le next_node de l'itération précédente *)

      (* Si on arrive au puit, on sait que ce n'est pas la node de fin grâce au if précédent donc on remonte *)
      | [] -> go_next gr work_gr n_fin (n::visited_nodes) h

      (* Il y a des arcs sortants pour la node sélectionnée *)
      | l ->  begin (* Printf.printf "visited_nodes : "; List.iter (Printf.printf "%d ") visited_nodes; *)
          match (find_aug_arc gr work_gr l n visited_nodes h) with

          (* Aucun arc augmentant pour cette node parmis les arcs sortants *)
          | None -> Printf.printf "Aucun arc augmentant trouvé\n%!";
            (* On va chercher dans h la prochaine node sur laquelle itérer *)
            (* Si il n'y en a plus, on a visité la source et l'algorithme termine *)
            go_next gr work_gr n_fin (n::visited_nodes) h

          (* On a trouvé un arc valide, on se déplace *)
          | Some (next_node, label) -> Printf.printf "DEPLACEMENT %d->%d\n%!" n next_node; find_aug_path gr work_gr next_node n_fin visited_nodes (stack h next_node)
        end
  in

  find_aug_path gr work_gr n_debut n_fin [] [n_debut]


(* Algorithme général : créé le graph de travail, convertit le graphe de référence en int graph et lance find_augmenting_path jusqu'au flot optimal *)

let fulkerson (gr:'a graph) (conversion: 'a -> int) (node_debut:id) (node_fin:id) = 
  let work_graph = gmap gr (fun x -> 0) in

  let ref_graph = gmap gr (conversion) in


  let rec recu_fulkerson (ref_gr: int graph) (work_gr: int graph) = 
    try 
      match (find_augmenting_path ref_gr work_gr node_debut node_fin) with
      | [] -> assert false
      | result -> recu_fulkerson ref_gr (update_work_graph ref_gr work_gr result)
    with
      Flot_optimal -> remove_negative_arcs work_gr

  in

  recu_fulkerson ref_graph work_graph