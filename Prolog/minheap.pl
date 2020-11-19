%%%% -*- Mode: prolog -*-

% new_heap/1 adds a new heap in the program's knowledge base

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.



% delete_heap/1 deletes the entire heap from the program's knownledge base

delete_heap(H) :-
	retractall(heap_entry(H, _, _, _),
	retractall(heap(H)).



% heapsize/2 true when H is an heap and S its size	

heap_size(H, S) :- heap(H, S).



% heap_empty/1 true if the heap H is empty

heap_empty(H) :- 
	heap_size(H, S),
	S = 0.



% heap_not_empty/1 true if the H is not empty thus heap_empty/1 cannot be proven

heap_not_empty(H) :- not(heap_empty(H)).



% heap_head/3 true when the heap entry H with the minimum key K is V

heap_head(H, K, V) :-
	findall(Key, heap_entry(H, _, Key, _), Keys),
	min_of(Keys, K).

	

% min_of/2 support predicate for heap_head/2, given a list of keys and a key K,
% the predicate is true if K is the minimum element

min_of([K], K).
min_of([K1 | Keys], K) :-
	K <= K1,
	min_of(Keys, K).



heap_extract(H, K, V) :-
	heap_size(H, S),
	S > 1, !,
	retractall(heap_entry(H, 1, K, V)),
	retractall(heap_entry(H, S, KTail, VTail)),
	assert(heap_entry(H, 1, KTail, VTail)),
	NewS is S + 1,
	retractall(heap(H, S)),
	assert(heap(H, NewS)),
	heapify(H, 1).

heap_decrease_key(H, P, K) :-
	heap_entry(H, P, OldK, V),
	OldK >= K,
	retractall(heap_entry(H, P, OldK, V)),
	assert(heap_entry(H, P, K, V)),
	
	heap_move_up(H, K, V).

heap_move_up(H, K, V) :- heap_entry(H, 1, K, V), !.

heap_move_up(H, K, V) :-
	heap_entry(H, P, K, V),
	P > 1,
	PPar is floor(i / 2),
	heap_entry(H, PPar, KPar, VPar),
	KPar < K, !.

heap_move_up(H, K, V) :- 
	heap_entry(H, P, K, V),
	P > 1,
	PPar is floor(i / 2),
	heap_entry(H, PPar, KPar, VPar),
	KPar > K, !,
	retractall(heap_entry(H, P, K, V)),
	retractall(heap_entry(H, PPar, KPar, VPar)),
	assert(heap_entry(H, PPar, K, V)),
	assert(heap_entry(H, P, KPar, VPar)),
	heap_move_up(H, K, V).
	
heap_insert(H, K, V) :-
	heap_size(H, S),
	NewS is S + 1,
	retractall(heap(H, S)),
	assert(heap(H, NewS)),
	assert(heap_entry(H, NewS, inf, V))
	heap_decrease_key(H, NewS, K).

heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, V),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left <= S,
	Right <= S,
	heap_entry(H, Left, KLeft, VLeft),
	heap_entry(H, Right, KRight, VRight),
	
	min_key([KRight, Right], [KLeft, Left], [KMinRL, MinRL]),
	min_key([K, P], [KMinRL, MinRL], [KMin, Min]),
	
	heapify_on_different(H, Min, P).


heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, V),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left <= S,
	Right > S,
	heap_entry(H, Left, KLeft, VLeft),
	
	min_key([K, P], [KLeft, Left], [KMin, Min]),
	
	heapify_on_different(H, Min, P).

heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, V),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left > S,
	Right <= S,
	heap_entry(H, Right, KRight, VRight),
	
	min_key([K, P], [KRight, Right], [KMin, Min]),
	
	heapify_on_different(H, Min, P)

heapify(H, P) :-
	heap_size(H, S),
	heap_entry(H, P, K, V),
	Left is P * 2,
	Right is (P * 2) + 1,
	Left <= S,
	Right <= S,
	heap_entry(H, Left, KLeft, VLeft),
	heap_entry(H, Right, KRight, VRight),
	
	min_key([KRight, Right], [KLeft, Left], [KMinRL, MinRL]),
	min_key([K, P], [KMinRL, MinRL], [KMin, Min]),
	
	heapify_on_different(H, Min, P)

heapify_on_different(H, P, P) :- !.

heapify_on_different(H, Min, P) :-
	Min \= P, !,
	heap_switch(H, Min, P),
	heapify(H, Min).

min_key([K1, P1], [K2, _], [K1, P1]) :- K1 < K2, !.
min_key([K1, _], [K2, P2], [K2, P2]) :- K1 > K2, !.

heap_switch(H, P, P) :- !.
heap_switch(H, P1, P2) :- 
	P1 \= P2,
	retractall(heap_entry(H, P1, K1, V1)),
	retractall(heap_entry(H, P2, K2, V2)),
	assert(heap_entry(H, P2, K1, V1)),
	assert(heap_entry(H, P1, K2, V2)).