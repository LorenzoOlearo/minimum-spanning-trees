%%%% -*- Mode: Prolog -*-

% Do not autoload from autoload libraries
% autoload(explicit).


% Load the Swipl's library that parses and generates CSV data
use_module(library(csv)).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1)
:- dynamic graph/1, vertex/2, arc/4.


%!	new_graph(+Graph:term) is det
%
%	Predicate that asserts a graph/1 Graph if not already existing
%	The predicate is always true
%
%	@arg Graph term to be asserted as graph
%
new_graph(G) :- graph(G), !.

new_graph(G) :- assert(graph(G)), !.


%!	delete_graph(+Graph:graph) is det
%
%	Predicate that retracts all arc/4, vertex/2 and graph/1 associated with
%	graph form the knowledge base
%	The predicate is always true
%
%	@arg Graph
%
delete_graph(G) :-
	retractall(arc(G, _, _, _)),
	retractall(vertex(G, _)),
	retractall(graph(G)).


%!	new_vertex(+Graph:graph, +Vertex:term) is semidet
%
%	Predicate that asserts a vertex/2 that associates Graph with Vertex if not
%	already associated
%	The predicate is false if Graph is not a graph, else is true
%
%	@arg Graph
%	@arg Vertex term to be asserted as vertex
%
new_vertex(G, V) :-
	vertex(G, V), !.

new_vertex(G, V) :-
	graph(G), !,
	assert(vertex(G, V)).


%!	graph_vertices(+Graph:graph, -Vertices:list) is semidet
%
%	Predicate that is true when Vertices contains every vertex/2 associated
%	with the graph Graph
%	The predicate is false if Graph is not a graph
%
%	@arg Graph
%	@arg Vertices list of vertex/2 associated with Graph, empty if Graph is an
%		empty graph
%
graph_vertices(G, Vs) :-
	graph(G),
	findall(vertex(G, V), vertex(G, V), Vs).


%!	list_vertices(+Graph:graph) is det
%
%	Predicate that prints listing of all vertex/2 associated with the graph
%	Graph
%	The predicate is always true
%
%	@arg Graph
%
list_vertices(G) :- listing(vertex(G, _)).


%!	new_arc(+Graph:graph, +Source:vertex, +Destination:vertex, +Weight:number) is semidet
%
%	Predicate that asserts an arc/4 from Source to Destination in the graph.
%	Graph with assigned weight Weight
%	The predicate retracts any preexisting arc/4 joining Source and
%	Destination
%
%	@arg Graph
%	@arg Source vertex the arc starts from
%	@arg Destination vertex the arc arrives in
%	@arg Weight weight of the arc
%
new_arc(G, U, V, Weight) :-
	graph(G),
	vertex(G, U),
	vertex(G, V),
	retractall(arc(G, U, V, _)),
	retractall(arc(G, V, U, _)),
	assert(arc(G, U, V, Weight)).


%!	new_arc(+Graph:graph, +Source:vertex, +Destination:vertex) is semidet
%
%	Assumes weight of the arc is 1
%
%	@arg Graph
%	@arg Source vertex the arc starts from
%	@arg Destination vertex the arc arrives in
%	@see new_arc/4

new_arc(G, U, V) :- new_arc(G, U, V, 1).


%!	graph_arcs(+Graph:graph, -Arcs:list) is semidet
%
%	Predicate that is true when Arcs is a list of all the arc/4 associated
%	with the graph Graph
%	The predicate is false when Graph is not a graph
%
%	@arg Graph
%	@arg Arcs list of arc/4 associated with Graph, empty if Graph is an empty
%		graph

graph_arcs(G, Arcs) :-
	graph(G),
	findall(arc(G, V, U, W), arc(G, V, U, W), Arcs).


%!	vertex_neighbors(+Graph:graph, +Vertex:vertex, -Neighbors:list) is semidet
%
%	Predicate that is true when Neighbors is the list of all the arc/4 that
%	are incident to Vertex
%	The predicate is false when Vertex is not a vertex associated with Graph
%	This predicate is to be used in an undirected graph context
%
%	@arg Graph
%	@arg Vertex
%	@arg Neighbors list of arc/4 that are incident to Vertex, with the oreder
% 		of the arguments arranged to have Vertex as the second argument
%		regardless of the order they are arranged in the knowledge base

vertex_neighbors(G, V, Neighbors) :-
	vertex(G, V),
	findall([G, V, N, W], arc(G, V, N, W), From),
	findall([G, V, X, W], arc(G, X, V, W), To),
	append(From, To, Ns),
	build_arcs_from_list(Ns, Neighbors).


%!	vertex_neighbors_oriented(+Graph:graph, +Vertex:vertex, -Neighbors:list)
%		is semidet
%
%	Predicate that is true when Neighbors is the list of all the arc/4 that
%	are have Vertex as tail
%	The predicate is false when Vertex is not a vertex associated with Graph
%	This predicate is to be used in a directed graph context
%
%	@arg Graph
%	@arg Vertex
%	@arg Neighbors list of arc/4 that have Vertex as tail

vertex_neighbors_oriented(G, V, Ns) :-
	vertex(G, V),
	findall(arc(G, V, N, W), arc(G, V, N, W), Ns).


%!	adjs(+Graph:graph, +Vertex:vertex, -Adjacents:list) is semidet
%
%	Predicate that is true when Adjacents is the list of all the vertex/2
%	that are joined to Vertex by an arc
%	This predicate is to be used in an undirected graph context
%
%	@arg Graph
%	@arg Vertex
%	@arg Adjacents list of vertex/2 joined to Vertex by an arcs

adjs(G, V, Vs) :-
	vertex(G, V),
	findall(vertex(G, N), arc(G, V, N, W), From),
	findall(vertex(G, X), arc(G, X, V, W), To),
	append(From, To, Vs).


%!	adjs_oriented(+Graph:graph, +Vertex:vertex, -Adjacents:list) is semidet
%
%	Predicate that is true when Adjacents is the list of all the vertex/2
%	that are head to an arc having Vertex as tail
%	This predicate is to be used in a directed graph context
%
%	@arg Graph
%	@arg Vertex
%	@arg Adjacents list of vertex/2 that are head of an arc having Vertex as
%		tail

adjs_oriented(G, V, Vs) :-
	vertex(G, V),
	findall(vertex(G, N), arc(G, V, N, _), Vs).


%!	list_arcs(+Graph:graph) is semidet
%
%	Predicate that prints the listing of arc/4 associated with G
%	The predicate is false when Graph is not a graph
%
%	@arg Graph

list_arcs(G) :- 
	graph(G),
	listing(arc(G, _, _, _)).


%!	list_graph(+Graph:graph) is semidet
%
%	Predicate that prints the listing of arcs/4, vertices/2
%	The predicate is false when Graph is not a graph
%
%	@args Graph

list_graph(G) :-
	graph(G),
	list_arcs(G),
	list_vertices(G).


%!	read_graph(+Graph:graph, +FileName:path) is semidet
%
%	Predicate that reads arc/4 of Graph from a tab separated csv file,
%	every arc will be written as a triple {Source Destination Weight}
%	omitting the term representing the graph
%	Graph will be asserted as graph if not already a graph
%	Every vertex present as end-point of the arc will be asserted as 
%	vertex of Graph
%	Every arc will be asserted as arc of Graph
%
%	@arg Graph
%	@arg FileName the path of the input csv file

read_graph(G, FileName) :-
	csv_read_file(FileName, Rows, [separator(0'\t)]),
	new_graph_from_rows(G, Rows).


%!	new_graph_from_rows(+Graph:graph, +Rows:rows) is semidet
%
%	Support predicate for read_graph/2
%	Predicate to create a graph G given a list in
%	the format[row(V, U, W)] where V is source vertex, U is destination 
%	vertex and W is weight of the arc between the two
%	This Predicate asserts graph/1, vertex/2, arc/4 as needed to represent
%	the arcs described in Rows
%
%	@arg Graph
%	@arg Rows list of row/3 elements

new_graph_from_rows(G, []) :- new_graph(G), !.

new_graph_from_rows(G, [row(V, U, W) | Rows]) :-
	number(W),
	new_graph(G),
	new_vertex(G, V),
	new_vertex(G, U),
	new_arc(G, V, U, W),
	new_graph_from_rows(G, Rows).


%!	write_graph(+Graph:graph, +FileName:path) is semidet
%
%	Predicate that writes arc/4 of Graph to a tab separated csv file,
%	every arc will be written as a triple {Source Destination Weight}
%	omitting the term representing the graph
%
%	@arg Graph
%	@arg FileName the path of the output csv file

write_graph(G, FileName, edges) :-
	!,
	write_arcs_in_rows(G, Rows),
	csv_write_file(FileName, Rows, [separator(0'\t)]).

write_graph(G, FileName, graph) :-
	!,
	graph_arcs(G, Arcs),
	write_graph(Arcs, FileName, edges).

write_graph(G, FileName) :- write_graph(G, FileName, graph).


%!	write_arcs_in rows(+Arcs:list, -Rows:list) is semidet
%
%	Support Predicate for write_graph/2
%	Predicate that is true when Rows is a list of row/3 such that
%	for every arc(G, V, U, W) in Arcs it contains a row(V, U, W)
%
%	@arg Arcs list of arc/4
%	@arg Rows list of row/3

write_arcs_in_rows([], []) :- !.

write_arcs_in_rows([Arc | Arcs], [Row | Rows]) :-
	Arc =.. [arc, _ | TRow],
	Row =.. [row | TRow],
	write_arcs_in_rows(Arcs, Rows).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1).
:- dynamic heap/2, heap_entry/4.



% new_heap/1 adds a new heap in the program's knowledge base

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.



% delete_heap/1 deletes the entire heap from the program's knownledge base

delete_heap(H) :-
	retractall(heap_entry(H, _, _, _)),
	retractall(heap(H, _)).



% heapsize/2 true when H is an heap and S its size

heap_has_size(H, S) :- heap(H, S).



% heap_empty/1 true if the heap H is empty

heap_empty(H) :-
	heap_has_size(H, S),
	S = 0.



% heap_not_empty/1 true if the H is not empty thus heap_empty/1 cannot be proven

heap_not_empty(H) :-
	heap_has_size(H, S),
	S > 0.



% heap_head/3 true when the minimum key in the heap H is K and V is the
% associated value

heap_head(H, K, V) :-
	heap_entry(H, 1, K, V).



% min_of/2 support predicate for heap_head/2, given a list of keys and a key K,
% the predicate is true if K is the minimum element

min_of([K], K) :- !.

min_of([K1 | Keys], K) :-
	K =< K1,
	min_of(Keys, K).



% heap_extract/3 true when K, V are key and value of the head of the heap.
% Changes the knowledge base removing the head from the heap

heap_extract(H, K, V) :-
	heap_has_size(H, 1), !,
	retract(heap_entry(H, 1, K, V)),
	retract(heap(H, 1)),
	assert(heap(H, 0)).

heap_extract(H, K, V) :-
	heap_has_size(H, S),
	S > 1, !,
	retract(heap_entry(H, 1, K, V)),
	heap_entry(H, S, KTail, VTail),
	retract(heap_entry(H, S, KTail, VTail)),
	assert(heap_entry(H, 1, KTail, VTail)),
	NewS is S - 1,
	retract(heap(H, S)),
	assert(heap(H, NewS)),
	heapify(H, 1).



% heap_decrease_key/3 support procedure for heap operations, moves a heap_entry
% from the position P to a new position according to the new key K

heap_decrease_key(H, P, NewKey) :-
	heap_entry(H, P, OldKey, V),
	heap_decrease_key(H, OldKey, NewKey, V).

heap_decrease_key(H, OldKey, NewKey, V) :-
	heap_entry(H, P, OldKey, V),
	OldKey > NewKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	heap_move_up(H, P).



% heap_move_up/2 support procedure for heap operations, moves a heap_entry,
% in a heap H at position P, up until needed according to its key

heap_move_up(_, 1) :- !.

heap_move_up(H, P) :-
	heap_entry(H, P, K, _),
	P > 1,
	PPar is floor(P / 2),
	heap_entry(H, PPar, KPar, _),
	KPar =< K, !.

heap_move_up(H, P) :-
	heap_entry(H, P, K, _),
	P > 1,
	PPar is floor(P / 2),
	heap_entry(H, PPar, KPar, _),
	KPar > K, !,
	heap_switch(H, P, PPar),
	heap_move_up(H, PPar).



% heap_insert/3 asserts a new heap_entry with key K and value V and keeps the
% heap-propriety

heap_insert(H, K, V) :-
	heap_has_size(H, S),
	NewS is S + 1,
	retract(heap(H, S)),
	assert(heap(H, NewS)),
	assert(heap_entry(H, NewS, K, V)),
	heap_move_up(H, NewS).



% heapify/2 restructures the heap taking for granted the two subtrees from P are
% already heaps

heapify(H, P) :-
	heap_has_size(H, S),
	heap_entry(H, P, _, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left > S,
	Right > S, !.

heapify(H, P) :-
	heap_has_size(H, S),
	heap_entry(H, P, K, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left =< S,
	Right > S, !,
	heap_entry(H, Left, KLeft, _),

	min_key([K, P], [KLeft, Left], [_, Min]),

	heapify_on_different(H, Min, P).

heapify(H, P) :-
	heap_has_size(H, S),
	heap_entry(H, P, K, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left > S,
	Right =< S, !,
	heap_entry(H, Right, KRight, _),

	min_key([K, P], [KRight, Right], [_, Min]),

	heapify_on_different(H, Min, P).

heapify(H, P) :-
	heap_has_size(H, S),
	heap_entry(H, P, K, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left =< S,
	Right =< S, !,
	heap_entry(H, Left, KLeft, _),
	heap_entry(H, Right, KRight, _),

	min_key([KRight, Right], [KLeft, Left], [KMinRL, MinRL]),
	min_key([K, P], [KMinRL, MinRL], [_, Min]),

	heapify_on_different(H, Min, P).



% heapify_on_different/3 support procedure for heap operations, calls an heapify
% if Min is different from P

heapify_on_different(H, Min, Min) :- ! , heap(H, _).

heapify_on_different(H, Min, P) :-
	Min \= P, !,
	heap(H, _),
	heap_switch(H, Min, P),
	heapify(H, Min).



% min_key/2 true when the third argument in form [K, P] is the couple of [K, P]
% with minimum K between [K1, P1], [K2, P2]

min_key([K1, P1], [K2, _], [K1, P1]) :- K1 =< K2, !.

min_key([K1, _], [K2, P2], [K2, P2]) :- K1 > K2, !.



% heap_switch/2 support procedure for heap operations, switches positions in H
% between the heap_entry at the given positions

heap_switch(H, P, P) :- !, heap(H, _).

heap_switch(H, P1, P2) :-
	P1 \= P2, !,
	heap(H, _),
	heap_entry(H, P1, K1, V1),
	heap_entry(H, P2, K2, V2),
	retract(heap_entry(H, P1, K1, V1)),
	retract(heap_entry(H, P2, K2, V2)),
	assert(heap_entry(H, P2, K1, V1)),
	assert(heap_entry(H, P1, K2, V2)).



% modify_key/4 replaces the OldKey associated with a vertex V
% in a heap H with the key NewKey without altering the min-heap property

modify_key(H, NewKey, OldKey, V) :-
	heap_decrease_key(H, OldKey, -inf, V),
	heap_extract(H, -inf, V),
	heap_insert(H, NewKey, V).



% list_heap/1 lists the current internal heap representation on the console

list_heap(H) :-
	heap(H, _),
	listing(heap(H, _)),
	listing(heap_entry(H, _, _, _)).



% heap_contains/3 is true when the heap H contains the entry with key K and
% value V

heap_contains(H, K, V) :-
	heap_entry(H, _, K, V).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1).
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
	update_keys(G, Ns),
	mst_prim(G).



% mst_get/3

% mst_get/3

mst_get(G, Source, PreorderTree) :-
	mst_get_neighbors(G, Source, Neighbors),
	mst_get_recurse(Neighbors, PreorderTree).



% mst_get_neighbors/3 support predicate for mst_get/3 could be incorporated in
% mst_get/3

mst_get_neighbors(G, Source, WSort) :-
	findall([G, Source, V, W] , (vertex_previous(G, V, Source),
								   arc(G, V, Source, W)), From),
	findall([G, Source, V, W], (vertex_previous(G, V, Source),
								   arc(G, Source, V, W)), To),
	append(From, To, List),
	build_arcs_from_list(List, Arcs),
	sort(3, @=<, Arcs, VSort),
	sort(4, @=<, VSort, WSort).



% build_arcs_from_list/2

build_arcs_from_list([], []) :- !.
build_arcs_from_list([Arg | Args], [Arc | Arcs]) :-
	Arc =.. [arc | Arg],
	build_arcs_from_list(Args, Arcs).



% mst_get_recurse/2 support predicate for mst_get, calls mst_get for all the
% adjacent nodes of Source given in as fouth argument

mst_get_recurse([], []) :- !.

mst_get_recurse([arc(G, S, V, W) | Ns], [arc(G, S, V, W) | MST]) :-
	mst_get(G, V, Tree),
	mst_get_recurse(Ns, Rest),
	append(Tree, Rest, MST).



% The support predicate init/4 initializes the heap H for the Prim's algorithm,
% takes a list containing all the vertices [V | Vs] and inserts each one of them
% as a heap entry for the heap H, when these entries are first added their key
% value is set to inf

init(H, G, [], Source) :-
	!,
	retract(vertex_key(G, Source, inf)),
	assert(vertex_key(G, Source, 0)),
	heap_decrease_key(H, inf, 0, Source),
	heap_extract(H, 0, Source),
	vertex_neighbors(G, Source, Ns),
	update_keys(H, Ns).

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

update_keys(_, []) :- !.

update_keys(H, [arc(G, Source, V, W) | Ns]) :-
	vertex_key(G, V, K),
	heap_contains(H, K, V),
	W < K, !,
	retract(vertex_key(G, V, K)),
	assert(vertex_key(G, V, W)),
	heap_decrease_key(H, K, W, V),
	retractall(vertex_previous(G, V, _)),
	assert(vertex_previous(G, V, Source)),
	update_keys(H, Ns).

update_keys(H, [arc(G, _, V, W) | Ns]) :-
	vertex_key(G, V, K),
	heap_contains(H, K, V),
	W >= K, !,
	update_keys(H, Ns).

update_keys(H, [_ | Ns]) :-
	update_keys(H, Ns).



% mst_reset/1

mst_reset(G) :-
	graph(G),
	delete_heap(G),
	retractall(vertex_key(G, _, _)),
	retractall(vertex_previous(G, _, _)).
