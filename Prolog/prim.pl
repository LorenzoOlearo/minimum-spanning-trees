%%%% -*- Mode: Prolog -*-

:-dynamic vertex_key/3, vertex_previous/3.

% For each vertex V in the graph G the predicate mst_prim/2 will add in the
% knowledge base the predicates vertex_key(G, V, K) and
% vertex_previous(G, V, K) representing a solution to the MST problem.

mst_prim(G, Source) :-
    vertices(G, Vs),
    init(h, G, Vs, Source),
    mst_prim(G).

mst_prim(_) :-
    heap_size(h, S),
    S = 0, !.

mst_prim(G) :-
    heap_size(h, S),
    S > 0, !,
    heap_extract(h, _, V),
    neighbors(G, V, Ns),
    update_keys(h, Ns, V),
    mst_prim(G).



% the support predicate init/4 initialize the heap H for the Prim's algorithm,
% takes a list containing all the vertices [V | Vs] and inserts each one of them
% as a heapy entry for the heap H, when these are first added their key is set
% to inf

init(H, G, [], Source) :-
    !,
    retract(vertex_key(G, Source, inf)),
    assert(vertex_key(G, Source, 0)),
    %modify_key(H, 0, inf, Source),
    heap_decrease_key(H, inf, 0, Source),

    heap_extract(H, 0, Source),
    neighbors(G, Source, Ns),

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
