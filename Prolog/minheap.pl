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

heap_empty(H) :- findall(heap_entry(H, _, _, _), heap(H), []).



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
