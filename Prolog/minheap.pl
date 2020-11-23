%%%% -*- Mode: prolog -*-

:- dynamic heap/2, heap_entry/4.


% new_heap/1 adds a new heap in the program's knowledge base

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.



% delete_heap/1 deletes the entire heap from the program's knownledge base

delete_heap(H) :-
	retractall(heap_entry(H, _, _, _)),
	retractall(heap(H, _)).



% heapsize/2 true when H is an heap and S its size

heap_size(H, S) :- heap(H, S).



% heap_empty/1 true if the heap H is empty

heap_empty(H) :-
	heap_size(H, S),
	S = 0.



% heap_not_empty/1 true if the H is not empty thus heap_empty/1 cannot be proven

heap_not_empty(H) :- not(heap_empty(H)).



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
	heap_size(H, 1), !,
	retract(heap_entry(H, 1, K, V)),
	retract(heap(H, 1)),
	assert(heap(H, 0)).

heap_extract(H, K, V) :-
	heap_size(H, S),
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

heap_decrease_key(H, P, K) :-
	heap_entry(H, P, OldK, V),
	OldK >= K,
	retract(heap_entry(H, P, OldK, V)),
	assert(heap_entry(H, P, K, V)),
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
	heap_size(H, S),
	NewS is S + 1,
	retract(heap(H, S)),
	assert(heap(H, NewS)),
	assert(heap_entry(H, NewS, K, V)),
	heap_move_up(H, NewS).



% heapify/2 restructures the heap taking for granted the two subtrees from P are
% already heaps

heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, _, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left > S,
	Right > S, !.



heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left =< S,
	Right > S, !,
	heap_entry(H, Left, KLeft, _),

	min_key([K, P], [KLeft, Left], [_, Min]),

	heapify_on_different(H, Min, P).



heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, _),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left > S,
	Right =< S, !,
	heap_entry(H, Right, KRight, _),

	min_key([K, P], [KRight, Right], [_, Min]),

	heapify_on_different(H, Min, P).



heapify(H, P) :-
	heap_size(H, S),
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



% modify_key/4

modify_key(H, NewKey, OldKey, V) :-
	heap_entry(H, P, OldKey, V),
	retract(heap_entry(H, P, OldKey, V)),
	assert(heap_entry(H, P, NewKey, V)),
	heapify(H, P).



% list_heap/1

list_heap(H) :-
	heap(H, _),
	listing(heap(H, _)),
	listing(heap_entry(H, _, _, _)).
