open Graph

(* Assuming two arcs cannot have the same beginning and end nodes, a path is described by the list of nodes *)
type g_path = id list

type heap = id list

(* Adds or removes an element from the heap *)
val stack: heap -> id -> heap
val unstack: heap -> id -> heap

(* Uses depth first search to find an augmenting path in the given graph *)
val find_augmenting_path: 'a graph -> g_path