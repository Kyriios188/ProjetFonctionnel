(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let rec clone_nodes gr = match gr with
  | [] -> []
  | x::t -> x.[0] :: clone_nodes t;;

ohlalalalalalala

let gmap gr f = assert false
let add_arc gr a b c = assert false