open Graph

type heap = id list

(* Adds or removes an element from the heap *)
val stack: heap -> id -> heap
val unstack: heap -> heap
val heap_top: heap -> id option
val print_heap: heap -> unit

(* Uses depth first search to find an augmenting path in the given graph *)
val find_augmenting_path: int graph -> int graph -> int -> heap
val update_work_graph: int graph -> int graph -> heap -> int graph