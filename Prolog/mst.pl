%%%% -*- Mode: Prolog -*-

%%%% mst.pl--
%%%%
%%%% Minimum Spanning Trees
%%%% Progetto gennaio 2021 (E1P) Linguaggi di Programmazione Anno Accademico
%%%% 2020-2021
%%%%
%%%% Gruppo composto da:
%%%%    Lorenzo Olearo, matricola ------
%%%%    Alessandro Riva, matricola ------



% Load the Swipl's library that parses and generates CSV data
:- use_module(library(csv)).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1)
:- dynamic

%!	graph(+Graph:term) is semidet
%
%	Predicate that is true when the term Graph is a graph

graph/1,


%!	vertex(+Graph:graph, +Vertex:vertex) is semidet
%
%	Predicate that is true when Vertex is a vertex in the graph Graph

vertex/2,



%!	arc(+Graph:graph, +Source:vertex, +Destination:vertex, +Weight:number)
%
%	Predicate that is true when Source and Destination are two vertices in
%	the graph Graph joined by an arc of weight Weight

arc/4.


%!	new_graph(+Graph:term) is det
%
%	Predicate that asserts a graph/1 Graph if not already existing
%	The predicate is always true
%
%	@arg Graph term to be asserted as graph

new_graph(G) :- graph(G), !.

new_graph(G) :- assert(graph(G)), !.


%!	delete_graph(+Graph:graph) is det
%
%	Predicate that retracts all arc/4, vertex/2 and graph/1 associated with
%	graph form the knowledge base <br>
%	The predicate is always true
%
%	@arg Graph graph object of the action

delete_graph(G) :-
	retractall(arc(G, _, _, _)),
	retractall(vertex(G, _)),
	retractall(graph(G)).


%!	new_vertex(+Graph:graph, +Vertex:term) is semidet
%
%	Predicate that asserts a vertex/2 that associates Graph with Vertex if
%	not already associated <br>
%	The predicate is false if Graph is not a graph, else is true
%
%	@arg Graph graph object of the action
%	@arg Vertex term to be asserted as vertex

new_vertex(G, V) :-
	vertex(G, V), !.

new_vertex(G, V) :-
	graph(G), !,
	assert(vertex(G, V)).


%!	graph_vertices(+Graph:graph, -Vertices:list) is semidet
%
%	Predicate that is true when Vertices contains every vertex/2 associated
%	with the graph Graph <br>
%	The predicate is false if Graph is not a graph
%
%	@arg Graph graph object of the action
%	@arg Vertices list of vertex/2 associated with Graph, empty if Graph is
%		an empty graph

graph_vertices(G, Vs) :-
	graph(G),
	findall(vertex(G, V), vertex(G, V), Vs).


%!	list_vertices(+Graph:graph) is det
%
%	Predicate that prints listing of all vertex/2 associated with the graph
%	Graph <br>
%	The predicate is always true
%
%	@arg Graph graph object of the action

list_vertices(G) :- listing(vertex(G, _)).


%!	new_arc(+Graph:graph, +Source:vertex,
%!	+Destination:vertex, +Weight:number) is semidet
%
%	Predicate that asserts an arc/4 from Source to Destination in
%	the graph <br>
%	Graph with assigned weight Weight <br>
%	The predicate retracts any preexisting arc/4 joining Source and
%	Destination
%
%	@arg Graph graph object of the action
%	@arg Source vertex the arc starts from
%	@arg Destination vertex the arc arrives in
%	@arg Weight weight of the arc

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
%	@arg Graph graph object of the action
%	@arg Source vertex the arc starts from
%	@arg Destination vertex the arc arrives in
%	@see new_arc/4

new_arc(G, U, V) :- new_arc(G, U, V, 1).


%!	graph_arcs(+Graph:graph, -Arcs:list) is semidet
%
%	Predicate that is true when Arcs is a list of all the arc/4 associated
%	with the graph Graph <br>
%	The predicate is false when Graph is not a graph
%
%	@arg Graph graph object of the action
%	@arg Arcs list of arc/4 associated with Graph, empty if Graph is an
%		empty graph

graph_arcs(G, Arcs) :-
	graph(G),
	findall(arc(G, V, U, W), arc(G, V, U, W), Arcs).


%!	vertex_neighbors(+Graph:graph, +Vertex:vertex, -Neighbors:list)
%!	is semidet
%
%	Predicate that is true when Neighbors is the list of all the arc/4 that
%	are incident to Vertex <br>
%	The predicate is false when Vertex is not a vertex associated with
%	Graph <br>
%	This predicate is to be used in an undirected graph context
%
%	@arg Graph graph object of the action
%	@arg Vertex end-point of the arcs in Neighbors
%	@arg Neighbors list of arc/4 that are incident to Vertex, with the
%		oreder of the arguments arranged to have Vertex as the second
%		argument regardless of the order they are arranged in the knowledge
%		base

vertex_neighbors(G, V, Neighbors) :-
	vertex(G, V),
	findall([G, V, N, W], arc(G, V, N, W), From),
	findall([G, V, X, W], arc(G, X, V, W), To),
	append(From, To, Ns),
	build_arcs_from_list(Ns, Neighbors).


%!	vertex_neighbors_oriented(+Graph:graph,
%!	+Vertex:vertex, -Neighbors:list) is semidet
%
%	Predicate that is true when Neighbors is the list of all the arc/4 that
%	are have Vertex as tail <br>
%	The predicate is false when Vertex is not a vertex associated with
%	Graph <br>
%	This predicate is to be used in a directed graph context
%
%	@arg Graph graph object of the action
%	@arg Vertex tail of the arcs in Neighbors
%	@arg Neighbors list of arc/4 that have Vertex as tail

vertex_neighbors_oriented(G, V, Ns) :-
	vertex(G, V),
	findall(arc(G, V, N, W), arc(G, V, N, W), Ns).


%!	adjs(+Graph:graph, +Vertex:vertex, -Adjacents:list) is semidet
%
%	Predicate that is true when Adjacents is the list of all the vertex/2
%	that are joined to Vertex by an arc <br>
%	This predicate is to be used in an undirected graph context
%
%	@arg Graph graph object of the action
%	@arg Vertex vertex the arcs in Adjacents are joined to
%	@arg Adjacents list of vertex/2 joined to Vertex by an arcs

adjs(G, V, Vs) :-
	vertex(G, V),
	findall(vertex(G, N), arc(G, V, N, W), From),
	findall(vertex(G, X), arc(G, X, V, W), To),
	append(From, To, Vs).


%!	adjs_oriented(+Graph:graph, +Vertex:vertex, -Adjacents:list) is semidet
%
%	Predicate that is true when Adjacents is the list of all the vertex/2
%	that are head to an arc having Vertex as tail <br>
%	This predicate is to be used in a directed graph context
%
%	@arg Graph graph object of the action
%	@arg Vertex vertex the arcs in Adjacents are joined to
%	@arg Adjacents list of vertex/2 that are head of an arc having Vertex
%		as tail

adjs_oriented(G, V, Vs) :-
	vertex(G, V),
	findall(vertex(G, N), arc(G, V, N, _), Vs).


%!	list_arcs(+Graph:graph) is semidet
%
%	Predicate that prints the listing of arc/4 associated with G <br>
%	The predicate is false when Graph is not a graph
%
%	@arg Graph graph object of the action

list_arcs(G) :-
	graph(G),
	listing(arc(G, _, _, _)).


%!	list_graph(+Graph:graph) is semidet
%
%	Predicate that prints the listing of arc/4, vertices/2 <br>
%	The predicate is false when Graph is not a graph
%
%	@arg Graph graph object of the action

list_graph(G) :-
	graph(G),
	list_arcs(G),
	list_vertices(G).


%!	read_graph(+Graph:graph, +FileName:path) is semidet
%
%	Predicate that reads arc/4 of Graph from a tab separated csv file,
%	every arc will be written as a triple {Source Destination Weight}
%	omitting the term representing the graph <br>
%	Graph will be asserted as graph if not already a graph <br>
%	Every vertex present as end-point of the arc will be asserted as
%	vertex of Graph <br>
%	Every arc will be asserted as arc of Graph
%
%	@arg Graph graph object of the action
%	@arg FileName the path of the input csv file

read_graph(G, FileName) :-
	read_graph(G, FileName, 0'\t).


%!	read_graph(+Graph:graph, +FileName:path, +Separator:number) is semidet
%
%	Predicate that reads arc/4 of Graph from a Separator separated csv file,
%	every arc will be written as a triple {Source Destination Weight}
%	omitting the term representing then the graph <br>
%	Graph will be asserted as graph if not already a graph <br>
%	The entire graph will be removed from the knowledge base if already
%	existing, a new graph will then be created from the CSV file. <br>
%	Every vertex present as end-point of the arc will be asserted as
%	vertex of Graph <br>
%	Every arc will be asserted as arc of Graph
%
%	@arg Graph graph object of the action
%	@arg FileName the path of the input csv file
%	@arg Separator the ASCII code correspoing to the separator of the csv file

read_graph(G, FileName, Separator) :-
	delete_graph(G),
	csv_read_file(FileName, Rows, [separator(Separator)]),
	new_graph_from_rows(G, Rows).


%!	new_graph_from_rows(+Graph:graph, +Rows:rows) is semidet
%
%	Support predicate for read_graph/2 <br>
%	Predicate to create a graph G given a list in
%	the format[row(V, U, W)] where V is source vertex, U is destination
%	vertex and W is weight of the arc between the two <br>
%	This Predicate asserts graph/1, vertex/2, arc/4 as needed to represent
%	the arcs described in Rows
%
%	@arg Graph graph object of the action
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
%	every arc will be written as a triple {Source, Destination, Weight}
%	omitting the term representing the graph
%
%	@arg Graph graph object of the action
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


%!	write_arcs_in_rows(+Arcs:list, -Rows:list) is semidet
%
%	Support Predicate for write_graph/2 <br>
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
:- dynamic

%!	heap(+Heap:term, -Size:number) is semidet
%
%	Predicate that is true when Heap is a heap of size Size

heap/2,


%!	heap_entry(+Heap:heap, -Position:number, +Key:number, -Value:term)
%!	is nondet
%
%	Predicate that is true when Value is stored in the heap Heap with key
%	Key and in position Position in the array representing the heap

heap_entry/4.



%!	new_heap(+Heap:term) is det
%
%	Predicate that asserts heap/2 associated with Heap if not already
%	existing<br>
%	This predicate is always true
%
%	@arg Heap the term to be asserted as heap

new_heap(H) :- heap(H, _), !.

new_heap(H) :- assert(heap(H, 0)), !.


%!	delete_heap(+Heap:heap) is det
%
%	Predicate that retracts heap/2 and all heap_entry/4 associated with
%	Heap<br>
%	This predicate is always true
%
%	@arg Heap heap object of the action

delete_heap(H) :-
	retractall(heap_entry(H, _, _, _)),
	retractall(heap(H, _)).


%!	heap_has_size(+Heap:heap, -Size:number) is semidet
%
%	Predicate that is true when Size is the size of the heap Heap
%
%	@arg Heap heap object of the action
%	@arg Size size of the heap

heap_has_size(H, S) :- heap(H, S).


%!	heap_empty(+Heap:heap) is semidet
%
%	Predicate that is true when Heap is a empty heap
%
%	@arg Heap heap object of the action

heap_empty(H) :-
	heap_has_size(H, S),
	S = 0.


%!	heap_not_empty(+Heap:heap) is semidet
%
%	Predicate that is true if the Heap is a non-empty heap
%
%	@arg Heap heap object of the action

heap_not_empty(H) :-
	heap_has_size(H, S),
	S > 0.


%!	heap_head(+Heap:heap, -Key:number, -Value:term) is semidet
%
%	Predicate that is true when the minimum key in the heap Heap is Key and
%	Value is the associated value
%
%	@arg Heap heap object of the action
%	@arg Key key of the first element of the heap
%	@arg Value value of the firt element of the heap

heap_head(H, K, V) :-
	heap_entry(H, 1, K, V).


%!	heap_extract(+Heap:heap, -Key:number, -Value:term) is semidet
%
%	Predicate that is true when Key, Value are key and value of the head
%	of the heap Heap<br>
%	This predicate changes the knowledge base removing the head from the
%	heap and restructuring the heap to mantain the heap property
%
%	@arg Heap heap object of the action
%	@arg Key key of the head of the heap
%	@arg Value value of the head of the heap

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


%!	heap_decrease_key(+Heap:heap, +Position:number, +NewKey:number)
%!	is semidet
%
%	Support predicate for heap operations<br>
%	Predicate that changes the key of the heap_entry/4 with position
%	Position and moves it to a new position according to NewKey if NewKey
%	is lesser then the preexisting key, else the predicate fails
%
%	@arg Heap heap object of the action
%	@arg Position index of the heap array indexing the heap_entry/4
%	@arg NewKey new value to be associated with the heap_entry/4

heap_decrease_key(H, P, NewKey) :-
	heap_entry(H, P, OldKey, V),
	heap_decrease_key(H, OldKey, NewKey, V).

heap_decrease_key(H, OldKey, NewKey, V) :-
	heap_entry(H, P, OldKey, V),
	OldKey > NewKey,
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	heap_move_up(H, P).


%!	heap_move_up(+Heap:heap, +Position:number) is semidet
%
%	Support predicate for heap operations<br>
%	Predicate that moves a heap_entry/4, in a heap Heap at position
%	Position, up until needed according to its key
%
%	@arg Heap heap object of the action
%	@arg Position index of the heap array indexing the heap_entry/4

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


%!	heap_insert(+Heap:heap, +Key:number, +Value:term) is semidet
%
%	Predicate that asserts a new heap_entry/4 with key Key and value
%	Value<br>
%	This predicate changes the knowledge base adding the new heap_entry/4
%	in the heap and restructuring the heap to mantain the heap property
%
%	@arg Heap heap object of the action
%	@arg Key the key of the new heap_entry/4
%	@arg Value the value of the new heap_entry/4

heap_insert(H, K, V) :-
	heap_has_size(H, S),
	NewS is S + 1,
	retract(heap(H, S)),
	assert(heap(H, NewS)),
	assert(heap_entry(H, NewS, K, V)),
	heap_move_up(H, NewS).


%!	heapify(+Heap:heap, +Position:number) is semidet
%
%	Predicate that restructures the heap taking for granted the two
%	subtrees from P are already heaps
%
%	@arg Heap heap object of the action
%	@arg Position index of the heap array to heapify

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


%!	heapify_on_different(+Heap:heap, +To_heapify:number, +Position:number)
%!	is semidet
%
%	Support predicate for heap operations<br>
%	Predicate that calls an heapify/2 on To_heapify if it is different from
%	Position
%
%	@arg Heap heap object of the action
%	@arg To_heapify index of the heap array to heapify
%	@arg Position generally the position previosly heapified

heapify_on_different(H, Min, Min) :- ! , heap(H, _).

heapify_on_different(H, Min, P) :-
	Min \= P, !,
	heap(H, _),
	heap_switch(H, Min, P),
	heapify(H, Min).


%	min_key([+Key1:number, +Position1:number],
%!			[+Key2:number, +Position2:number]
%!			[-KeyMin:number, -PositionMin:number]) is det
%
%	Support predicate for heap operations<br>
%	Predicate that is true when [KeyMin, PositionMin] is the couple with
%	minimum key between [Key1, Position1] and [Key2, Position2]

min_key([K1, P1], [K2, _], [K1, P1]) :- K1 =< K2, !.

min_key([K1, _], [K2, P2], [K2, P2]) :- K1 > K2, !.


%!	heap_switch(+Heap:heap, +Position1:number, +Position2:number)
%!	is semidet
%
%	Support predicate for heap operations<br>
%	Predicate that switches positions in Heap
%	between the heap_entry/4 at the given positions
%
%	@arg Heap heap object of the action
%	@arg Position1 position of the first heap_entry/4
%	@arg Position2 position of the second heap_entry/4

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


%!	modify_key(+Heap:heap, +NewKey:number, +OldKey:number, +Value:term)
%!	is nondet
%
%	Predicate replaces the a heap_entry with key OldKey and value Value
%	with one with NewKey as key and the same Value<br>
%	This predicate changes the knowledge base changing the heap_entry/4 in
%	the heap and restructuring the heap to mantain the heap property<br>
%	This predicate is meant for a context with unique couple {key, value},
%	else it is expected to work fine and change all the unifing
%	heap_entry/4 but the feature is untested<br>
%	This predicate is meant to be used only for NewKey > OldKey for
%	heap_decrease_key/3 is way more efficient in NewKey =< OldKey use case
%
%	@arg Heap heap object of the action
%	@arg NewKey key to be assigned at heap_entry/4
%	@arg OldKey key of preexisting heap_entry/4
%	@arg Value	value of preexisting heap_entry/4

modify_key(H, NewKey, OldKey, V) :-
	heap_decrease_key(H, OldKey, -inf, V),
	heap_extract(H, -inf, V),
	heap_insert(H, NewKey, V).


%!	list_heap(+Heap:heap) is semidet
%
%	Predicate that prints the listing of heap/2 and heap_entry/4 associated
%	with heap Heap
%
%	@arg Heap heap object of the action

list_heap(H) :-
	heap(H, _),
	listing(heap(H, _)),
	listing(heap_entry(H, _, _, _)).



%!	heap_contains(+Heap:heap, +Key:number, +Value:term) is semidet
%
%	Predicate that is true when the heap Heap contains the heap_entry/4
%	with key Key and value Value
%
%	@arg Heap heap object of the action
%	@arg Key key of the heap_entry/4
%	@arg Value value of the heap_entry/4

heap_contains(H, K, V) :-
	heap_entry(H, _, K, V).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1).
:- dynamic

%!	vertex_key(+Graph:graph, +Vertex:vertex, +Weight:number)
%
%	predicate that represents the evaluation of the cost to reach a vertex
%	according to the Prim's algorithm
%
%	@arg Graph the graph containing Vertex
%	@arg Vertex the name of the evaluated vertex
%	@arg Weight the cost associeated to the vertex

vertex_key/3,


%!	vertex_previous(+Graph:graph, +Vertex:vertex, +Previous:vertex)
%
%	predicate that represents the previous vertex needed to reach a vertex
%	Vertex according to the Prim's algorithm
%
%	@arg Graph the graph containing Vertex and Previous
%	@arg Vertex the name of the evaluated vertex
%	@arg Previous the vertex to witch Vertex is connected

vertex_previous/3.


%!	mst_prim(+Graph:graph, +Source:vertex) is semidet
%
%	Predicate that determines the minimum spanning tree of a graph Graph
%	using Source as start vertex for Prim's algorithm<br>
%	This predicate asserts vertex_key/3 and vertex_previous/3 describing
%	the mst
%
%	@arg Graph the graph object of the action
%	@arg Source the starting vertex for Prim's algorithm

mst_prim(G, Source) :-
	mst_reset(G),
	graph_vertices(G, Vs),
	init(G, G, Vs, Source),
	mst_prim(G).


%!	mst_prim(+Graph:graph) is semidet
%
%	Support predicate for mst_prim/2<br>
%	Predicate that solves the Prim's algorithm after intialization init/4

mst_prim(G) :-
	heap_has_size(G, S),
	S = 0, !.

mst_prim(G) :-
	heap_has_size(G, S),
	S > 0,
	heap_head(G, _, V),
	findall(vertex_previous(G, V, _), vertex_previous(G, V, _), []), !,
	delete_heap(G).

mst_prim(G) :-
	heap_has_size(G, S),
	S > 0,
	heap_head(G, _, V),
	findall(vertex_previous(G, V, _), vertex_previous(G, V, _), [_ | _]), !,
	heap_extract(G, _, V),
	vertex_neighbors(G, V, Ns),
	update_keys(G, Ns),
	mst_prim(G).


%!	mst_get(+Graph:graph, +Source:vertex, -PreorderTree:list) is semidet
%
%	Predicate that is true when PreorderTree is the list of arc/4
%	composing the minimum spanning tree of the graph Graph starting from
%	Source<br>
%	This predicate is supposed to be called after mst_prim(Graph, Source)
%	for it's based on its assertions, notice that the Source vertex has to
%	be the same to avoid unexpected behaviour
%
%	@arg Graph the graph object of the action
%	@arc Source the starting vertex for Prim's algorithm
%	@arg PreorderTree arc/4 enocuntered during a preoder visit of the tree

mst_get(G, Source, PreorderTree) :-
	mst_get_neighbors(G, Source, Neighbors),
	mst_get_recurse(Neighbors, PreorderTree).


%!	mst_get_neighbors(+Graph:graph, +Source:vertex, -Neighbors:list)
%!	is semidet
%
%	Support predicate for mst_get/3<br>
%	Predicate that finds the list Neighbors arc/4 of a Source vertex
%	according to the minimum spanning tree of the graph Graph
%
%	@arg Graph the graph object of the action
%	@arg Source the vertex object of the action
%	@arg	Neighbors list of neighbors arc/4 of Source

mst_get_neighbors(G, Source, WSort) :-
	findall([G, Source, V, W] , (vertex_previous(G, V, Source),
								 arc(G, V, Source, W)), From),
	findall([G, Source, V, W], (vertex_previous(G, V, Source),
								arc(G, Source, V, W)), To),
	append(From, To, List),
	build_arcs_from_list(List, Arcs),
	sort(3, @=<, Arcs, VSort),
	sort(4, @=<, VSort, WSort).


%!	build_arcs_from_list(+List:list, -Arcs:list) is semidet
%
%	Support predicate for graph operations
%	Predicate that is true when each element [G, V, U, W] of List
%	corresponds to an arc(G, V, U, W) with the same position in Arcs
%
%	@arg List list of [G, V, U, W] arguments of arc/4
%	@arg	Arcs list of composed arc/4

build_arcs_from_list([], []) :- !.
build_arcs_from_list([Arg | Args], [Arc | Arcs]) :-
	Arc =.. [arc | Arg],
	build_arcs_from_list(Args, Arcs).


%!	mst_get_recurse(+Arcs:list, -PreorderTree:list) is semidet
%
%	Support predicate for mst_get/3<br>
%	Predicate that is true when PreorderTree is the list of arc/4 visited
%	in the minimum spanning tree in a preorder visit, with Arcs being
%	the neighbors arcs
%
%	@arg Arcs list of arc/4
%	@arg PreorderTree arc/4 enocuntered during a preoder visit of the tree

mst_get_recurse([], []) :- !.

mst_get_recurse([arc(G, S, V, W) | Ns], [arc(G, S, V, W) | MST]) :-
	mst_get(G, V, Tree),
	mst_get_recurse(Ns, Rest),
	append(Tree, Rest, MST).


%!	init(+Heap:term, +Graph:graph, +Vertices:list, +Source:vertex)
%!	is semidet
%
%	Support predicate for mst_prim/2<br>
%	Predicate that initializes Heap as a heap for the Prim's algorithm,
%	with a list Vertices containing all the vertex/2 of a graph Graph<br>
%	This predicate asserts a heap_entry/4 for each vertex assigning inf as
%	key except for the Source vertex which is given key 0
%
%	@arg	Heap term to be asserted as heap/2
%	@arg	Graph the graph object of the action
%	@arg Vertices list of vertex/2 in Graph
%	@arg Source starting vertex for Prim's algorithm

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


%!	update_keys(+Heap:heap, +Arcs:list) is semidet
%
%	Support predicate for mst_prim/2<br>
%	Predicate that takes a list of arc/4 Arcs with a common head in the
%	graph Graph and asserts a new vertex_key/3, retracting a
%	pre-exisisting one, for each arc/4 in arcs if the weight of the arc/4
%	is less than the one in the existing vertex_key/3 and if the
%	vertex_key/3 is still in the heap Heap<br>
%	The predicate also updates the key in the heap
%
%	@arg Heap the heap containing the verices
%	@arg Arcs list of arcs used to update the keys

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


%!	mst_reset(+Graph:graph) is semidet
%
%	Predicate that retracts all data asserted by a mst_prim/2<br>
%	This predicate retracts all vertex_key/3, vertex_previous/3 and
%	deletes the heap
%
%	@arg Graph the graph object of the action

mst_reset(G) :-
	graph(G),
	delete_heap(G),
	retractall(vertex_key(G, _, _)),
	retractall(vertex_previous(G, _, _)).


%%%% end of file -- mst.pl--
