open Gfile;;
open Printf;;
open Graph;;
open Tools;;
open Fulkerson;;


let () =



  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  (* graph est un string graph *)
  let graph = from_file infile  in

  let () = export "graph_entree" graph in

  let graph_resultat = fulkerson graph int_of_string _source _sink in

  (* On affiche le résultat au format string *)
  let () = export outfile (gmap graph_resultat string_of_int) in

  ()