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

let print_heap h = List.iter (Printf.printf "%d ") h; Printf.printf "\n%!"

(* 
TODO list 
_ajouter le cas où on parcours un arc entrant (on remonte)
_avec ce cas, utiliser le successeur pour pas revenir en arrière
*)

(* 
On a une liste type [5; 4; 2; 0] qui retrace le chemin 0->2->4->5 
et on veut metre à jour les arcs correspondants avec le bon nombre
*)
let update_work_graph ref_gr w_gr heap =

  let rec find_update_value (ref_gr:int graph) (w_gr: int graph) (h:heap) (acu:int) = match h with
    | [] | [_] -> acu (* Si ya 0 ou 1 éléments on a parcouru tous les arcs *)
    | x::t -> begin 
        let y = List.hd t in
        (* Printf.printf "Augmentation %d->%d\n%!" x y; *)

        match [(find_arc ref_gr x y); (find_arc w_gr x y)] with
        (* Si cet arc est plus restictif que les précédents, on baisse la valeur d'augmentation *)
        | [Some ref_label; Some work_label] -> (* Printf.printf "On peut augmenter de %d à %d\n%!" work_label ref_label; *)
          if ref_label - work_label < acu then find_update_value ref_gr w_gr t (ref_label-work_label) 
          else find_update_value ref_gr w_gr t acu

        | _ -> failwith "Les deux graphs sont incompatibles" (* Si ya un None c'est qu'on a pas trouvé l'arc dans un graph *)
      end

  in

  let rec update_graph acu_gr h update_value = match h with
    | [] | [_] -> acu_gr
    | x::t -> let y = List.hd t in update_graph (add_arc acu_gr x y update_value) t update_value
  in

  let rev_heap = (List.rev heap) in
  (* On part du principe qu'il existe une arête de flot inférieur à 99999 dans le chemin parcouru *)
  let value = find_update_value ref_gr w_gr rev_heap 99999 in 

  Printf.printf "Valeur d'augmentation trouvée : %d\n%!" value;

  update_graph w_gr rev_heap value



(* Ne modifie pas le graph donné, donne juste un chemin augmentant *)
let find_augmenting_path (gr:int graph) (work_gr:int graph) (n_debut:id)= 

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