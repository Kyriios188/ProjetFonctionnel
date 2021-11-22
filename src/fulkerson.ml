open Graph


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


let find_augmenting_path (gr:'a graph) (n1:id) (n2:id) (current) = 

  (* On créé une liste de nodes visitées, une pile et une valeur d'augmentation *)
  let rec find_aug_path gr n n2 visited_nodes h augment = match (out_arcs gr n1) with

    (* On regarde si il y a des arcs sortants pour la node actuelle. *)
    (* Si non, on regarde la prochaine node dans la pile *)
    | [] -> 
      begin
        match (heap_top (unstack h)) with
        | None -> failwith "jsp quoi faire là"
        | Some x -> find_aug_path gr x n2 (x::visited_nodes) (unstack h) augment
      end
    (* *)
    | x::t -> 
      begin
        match x with
      end

  in
  find_aug_path gr [] []