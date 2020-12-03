%%%% -*- Mode: Prolog -*-

:- dynamic vertex_key/3, vertex_previous/3.

% For each vertex V in the graph G the predicate mst_prim/2 will add in the
% knowledge base the predicates vertex_key(G, V, K) and
% vertex_previous(G, V, K) representing a solution to the MST problem.

mst_prim(G, Source) :-
	mst_reset(G),
	graph_vertices(G, Vs),
	init(G, G, Vs, Source),
	mst_prim(G).

mst_prim(G) :-
	heap_has_size(G, S),
	S = 0, !.

mst_prim(G) :-
	heap_has_size(G, S),
	S > 0, !,
	heap_extract(G, _, V),
	vertex_neighbors(G, V, Ns),
	update_keys(G, Ns, V),
	mst_prim(G).



% mst_get/3

mst_get(G, Source, PreorderTree) :-
	findall(V, vertex_previous(G, V, Source), Vs),
	sort(Vs, Sorted_Vs),
	mst_get_recurse(G, Source, PreorderTree, Sorted_Vs).



% mst_get_recurse/4 support predicate for mst_get, calls mst_get for all the
% adjacent nodes of Source given in as fouth argument

mst_get_recurse(_, _, [], []) :- !.

mst_get_recurse(G, Source, [arc(G, Source, V, W) | Rest], [V | Vs]) :-
	arc(G, Source, V, W), !,
	mst_get(G, V, PreorderTree),
	mst_get_recurse(G, Source, Others, Vs),
	append(PreorderTree, Others, Rest).

mst_get_recurse(G, Source, [arc(G, V, Source, W) | Rest], [V | Vs]) :-
	arc(G, V, Source, W), !,
	mst_get(G, V, PreorderTree),
	mst_get_recurse(G, Source, Others, Vs),
	append(PreorderTree, Others, Rest).



% The support predicate init/4 initializes the heap H for the Prim's algorithm,
% takes a list containing all the vertices [V | Vs] and inserts each one of them
% as a heap entry for the heap H, when these entries are first added their key
% value is set to inf

init(H, G, [], Source) :-
	!,
	retract(vertex_key(G, Source, inf)),
	assert(vertex_key(G, Source, 0)),
	%modify_key(H, 0, inf, Source),
	heap_decrease_key(H, inf, 0, Source),

	heap_extract(H, 0, Source),
	vertex_neighbors(G, Source, Ns),

	update_keys(H, Ns, Source).

init(H, G, [V | Vs], Source) :-
	new_heap(H),
	V =.. [vertex, G, Val],
	heap_insert(H, inf, Val),
	assert(vertex_key(G, Val, inf)),
	init(H, G, Vs, Source).


% The support predicate update_keys/3 takes a list of arcs [N | Ns] in the graph
% G directed from the vertex Source to V, for each of the neighbors vertices of
% Source, the predicate will update their respective key to the weight W of that
% specific arc, if there is no connection between the two vertices the
% respective key in the heap entry will remains set to inf.

update_keys(_, [], _) :- !.

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, Source, V, W],
	vertex_key(G, V, K),
	heap_contains(H, K, V),
	W < K, !,
	retract(vertex_key(G, V, K)),
	assert(vertex_key(G, V, W)),

	%modify_key(H, W, K, V),
	heap_decrease_key(H, K, W, V),

	retractall(vertex_previous(G, V, _)),
	assert(vertex_previous(G, V, Source)),
	update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, V, Source, W],
	vertex_key(G, V, K),
	heap_contains(H, K, V),
	W < K, !,
	retract(vertex_key(G, V, K)),
	assert(vertex_key(G, V, W)),

	%modify_key(H, W, K, V),
	heap_decrease_key(H, K, W, V),

	retractall(vertex_previous(G, V, _)),
	assert(vertex_previous(G, V, Source)),
	update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, Source, V, W],
	vertex_key(G, V, K),
	W >= K, !,
	update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, V, Source, W],
	vertex_key(G, V, K),
	W >= K, !,
	update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, Source, V, _],
	vertex_key(G, V, K),
	not(heap_contains(H, K, V)), !,
	update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
	N =.. [arc, G, V, Source, _],
	vertex_key(G, V, K),
	not(heap_contains(H, K, V)), !,
	update_keys(H, Ns, Source).



% mst_reset/1

mst_reset(G) :-
	graph(G),
	delete_heap(G),
	retractall(vertex_key(G, _, _)),
	retractall(vertex_previous(G, _, _)).
