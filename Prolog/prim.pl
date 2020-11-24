%%%% -*- Mode: Prolog -*-

:-dynamic vertex_key/3, previous/3.


mst_prim(G, Source) :-
  not(vertex_key(G, _, 0)), !,
  vertices(G, Vs),
  init(h, G, Vs, Source),
  mst_prim(G, Source).

mst_prim(G, Source) :-
  vertex_key(G, Source, _), !,
  heap_extract(h, _, V),
  neighbors(G, V, Ns),
  update_keys(h, Ns, V),
  mst_prim(G, V).

init(H, G, [], Source) :-
  !,
  retract(vertex_key(G, Source, inf)),
  assert(vertex_key(G, Source, 0)),
  modify_key(H, 0, inf, Source),
  heap_extract(H, 0, Source),
  neighbors(G, Source, Ns),
  update_keys(H, Ns, Source).

init(H, G, [V | Vs], Source) :-
  new_heap(H),
  V =.. [vertex, G, Val],
  heap_insert(H, inf, Val),
  assert(vertex_key(G, Val, inf)),
  init(H, G, Vs, Source).


update_keys(_, [], _) :- !.

update_keys(H, [N | Ns], Source) :-
  N =.. [arc, G, Source, V, W],
  vertex_key(G, V, K),
  W < K, !,
  retract(vertex_key(G, V, K)),
  assert(vertex_key(G, V, W)),

  modify_key(H, W, K, V),

  retractall(previous(G, V, _)),
  assert(previous(G, V, Source)),
  update_keys(H, Ns, Source).

update_keys(H, [N | Ns], Source) :-
  N =.. [arc, G, V, Source, W],
  vertex_key(G, V, K),
  W < K, !,
  retract(vertex_key(G, V, K)),
  assert(vertex_key(G, V, W)),

  modify_key(H, W, K, V),

  retractall(previous(G, V, _)),
  assert(previous(G, V, Source)),
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
