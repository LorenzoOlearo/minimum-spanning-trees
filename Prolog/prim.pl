mst_prim(G, Source) :-
	new_heap(h),
	neighbors(G, Source, Ns),
	heap_add_neighbors(h, Source, Ns)
	heap_extract(h, K, N),
	add_vertex_key(K, N, Source, Destination),
	mst_prim(G, Destination).
	
heap_add_neighbors(H, _, []) :- heap(H, _), !. 
	
heap_add_neighbors(H, Source, [N | Ns]) :-
	N =.. [arc, Graph, U, Source, W],
	previous(Graph, Source, U), !,
	heap_add_neighbors(H, Source, Ns).
	
heap_add_neighbors(H, Source, [N | Ns]) :-
	N =.. [arc, Graph, Source, U, W],
	previous(Graph, Source, U), !,
	heap_add_neighbors(H, Source, Ns).
	
heap_add_neighbors(H, Source, [N | Ns]) :-
	N =.. [arc, Graph, Source, Source, W], !,
	heap_add_neighbors(H, Source, Ns).
	
heap_add_neighbors(H, Source, [N | Ns]) :-
	N =.. [arc, Graph, U, Source, W],
	U \= Source, !,
	heap_insert(H, W, N),
	heap_add_neighbors(H, Source, Ns).
	
add_vertex_key(K, N, Source, Destination) :-
	N =.. [arc, Graph, Destination, Source, K],
	assert(vertex_key(Graph, Destination, K)).