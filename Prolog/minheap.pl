%%%% -*- Mode: prolog -*-

new_heap(H) :- heap(H, _), !.
new_heap(H) :- assert(heap(H, 0)), !.

delete_heap(H) :-
	retractall(heap_entry(H, _, _, _),
	retractall(heap(H)).
	
heap_size(H, S) :- heap(H, S).

heap_empty(H) :- findall(heap_entry(H, _, _, _), heap(H), []).

heap_not_empty(H) :- findall(heap_entry(H, _, _, _), heap(H), [_ | _]).

heap_head(H, K, V) :-
	findall(Key, heap_entry(H, _, Key, _), Keys),
	min_of(Keys, K).
	
min_of([K], K).
min_of([K1 | Keys], K) :-
	K <= K1,
	min_of(Keys, K).

