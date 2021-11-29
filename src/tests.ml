(* ///////////////////////// *)
(* ///// TESTS EN COURS /////*)
(* ///////////////////////// *)





(* ///////////////////////// *)
(* ///// TESTS REUSSIS ///// *)
(* ///////////////////////// *)

(* Test find_augmenting_path *)

(* graph est un string graph *)
let graph = from_file infile  in
(* On créé un work_graph format int graph *)
let work_graph = gmap graph (fun x -> 0) in
(* On créé le graph de référence format int graph *)
let ref_graph = gmap graph (int_of_string) in
let test = find_augmenting_path ref_graph work_graph 0 5 in
print_heap test;
()

(* Test update_work_graph *)
(* Open file *)
(* graph est un string graph *)
let graph = from_file infile  in
let work_graph = gmap graph (fun x -> 0) in
let ref_graph = gmap graph (int_of_string) in
let test_heap = find_augmenting_path ref_graph work_graph 0 in
let updated_work_graph = update_work_graph ref_graph work_graph test_heap in
let () = export outfile (gmap updated_work_graph string_of_int) in
()

(* Test work_graph -> work_graph.svg dans le dossier graphs *)

(* convertit aussi le graph en int graph au passage et pas besoin de faire la copie *)
let work_graph = gmap graph_ref (fun x -> 0) in 
(* On repasse en string pour l'affichage (on aurait put faire fun x -> "0" et ne pas écrire cette ligne) *)
let str_work_graph = gmap work_graph (string_of_int) in 
let () = export outfile str_work_graph in
()

(* Tests heap *)

let h1 = [1; 2] in 
let h2 = stack h1 4 in
let h3 = stack h2 5 in 
let h4 = unstack h3 in 
let h5 = stack h4 12 in
print_heap h5;