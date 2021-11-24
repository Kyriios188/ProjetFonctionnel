open Graph
open Tools


type g_path = id list


type heap = id list
exception Empty_heap of string

(* fulkerson.ml *)
let stack (h:heap) (n:id) = n::h
let unstack (h:heap) = match h with
  | [] -> raise (Empty_heap "Cannot unstack empty heap")
  | x::t -> t
let heap_top (h:heap) = match h with
  | [] -> None
  | x::t -> Some x

let print_heap h = List.iter (Printf.printf "%d ") h

(* 
TODO list 
_ajouter le cas où on parcours un arc entrant (on remonte)
_avec ce cas, utiliser le successeur pour pas revenir en arrière
*)


(* Ne modifie pas le graph donné, donne juste un chemin augmentant *)
let find_augmenting_path (gr:int graph) (work_gr:int graph) (n_debut:id)= 

  Printf.printf "Début de l'algorithme\n";

  (* Prend une liste d'arcs en entrée et doit trouver Some arc valide ou retourner None *)
  let rec find_aug_arc (gr:int graph) (work_gr: int graph) arc_list begin_node visited_nodes = match arc_list with
    | [] -> None

    (* On décompose l'arc sortant en node d'arrivée et label *)
    | (next_node, work_label)::t -> begin
        Printf.printf "Je regarde la node d'arrivée %d\n%!" next_node;
        match (find_arc gr begin_node next_node) with
        | None -> failwith "Le graph de travail n'est pas compatible avec le graph de référence"
        (* On vérifie si l'arc augmente la valeur et si la node d'arrivée n'est pas marquée *)
        | Some label_arc_ref -> Printf.printf "On peut augmenter de %d\n%!" (label_arc_ref - work_label);
          if label_arc_ref - work_label >= 1 && not (List.mem next_node visited_nodes) then Some (next_node, work_label)
          (* L'arc n'est pas valide *)
          else find_aug_arc gr work_gr t begin_node visited_nodes
      end

  in

  (* On créé une liste de nodes visitées, une pile et une valeur d'augmentation *)
  let rec find_aug_path gr work_gr n visited_nodes h = match (out_arcs work_gr n) with

    (* La seule node qui n'a pas d'arc sortants c'est le puits *)
    | [] -> stack h n (* On renvoie le puit avec, c'est n2 et n *)

    (* Il y a des arcs sortants pour la node sélectionnée *)
    | l ->  begin 
        match (find_aug_arc gr work_gr l n visited_nodes) with
        (* Aucun arc augmentant pour cette node *)
        | None -> begin
            Printf.printf "Aucun arc augmentant trouvé partant de %d\n%!" n;
            (* On prend la prochaine node à parcourir dans l'ordre *)
            let next_node = (heap_top (unstack h)) in
            match next_node with
            | None -> failwith "Le flot est déjà optimal" (* On a atteint la source et on a essayé de la dépiler i.e. la source est visitée *)
            | Some x -> find_aug_path gr work_gr x (x::visited_nodes) (unstack h)
          end
        (* On a trouvé un arc valide, on se déplace *)
        | Some (next_node, label) -> Printf.printf "trouvé arc %d->%d valide\n%!" n next_node; find_aug_path gr work_gr next_node visited_nodes (stack h n)
      end
  in

  find_aug_path gr work_gr n_debut [] []