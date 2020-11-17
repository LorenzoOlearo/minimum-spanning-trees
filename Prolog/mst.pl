%%%% -*- Mode: Prolog -*-

%   TODO: check new_vertex/2 not asserting the new vertex in the knowledge base.
%   TODO: check what new_graph/1 is asserting in the knowledge base.


% Informs the interpreter that the definition of the predicate(s) may change
% during execution (using assert/1 and/or retract/1).
:- dynamic graph/1, vertex/2, arc/4.



% graph/1 identifies a graph

graph(_).



% arc/1 identifies an arc

arc(_, _, _, _).


% vertex/1 identifies a vertex

vertex(_, _).



% new_graph/1 Adds a graph to the knowledge base if not already existing.

new_graph(G) :- graph(G), !.

new_graph(G) :- assert(graph(G)), !.


% delete_graph/1 Removes a graph and all his nodes and arches form the knowledge
% base

delete_graph(G) :-
    retractall(arc(G, _, _, _)), retractall(vertex(G, _)), retractall(graph(G)).



% new_vertex/2 Adds a vertex V to a graph in the knowledge base. false if the
% graph is not in the knowledge base

new_vertex(G, V) :-
    graph(G), vertex(G, V), !.

new_vertex(G, V) :- graph(G), assert(vertex(G, V)), !.



% vertices/2 True when Vs is a list of every vertex in G

vertices(G, Vs) :- findall(V, vertex(G, V), Vs).



% list_vertices/1 Prints the list of vertices in a graph G

list_vertices(G) :- listing(vertex(G, _)).



% new_arc/4 adds a weighted arc (an edge according to graph theory) to a graph G
% in the knowledge base

new_arc(G, U, V, Weight) :-
    graph(G), vertex(G, U), vertex(G, V), arc(G, U, V, Weight), !.

new_arc(G, U, V, Weight) :-
    graph(G), vertex(G, U), vertex(G, V), assert(arc(G, U, V, Weight)), !.


% new_arc/3 adds a weighted arc with weight 1 (an edge according to graph
% theory) to a graph G in the knowledge base

new_arc(G, U, V) :- new_arc(G, U, V, 1).



% arcs/2 true when Es is a list of every arcs in a graph G

% arcs(G, Es) :- findall(arc(G, _, _, _), arc(G, _, _, _), Es).
% arcs(G, Es) :- findall(arc(G, V, U, W), arc(G, V, U, W), Es).
arcs(G, Es) :- findall(arc(G, _, _, _), arc(G, _, _, _), Es).
% probably the second one works better, not sure



% neighbors/3 true when V is a vertex in G and Ns is a list of all the arcs
% from V

neighbors(G, V, Ns) :-
    vertex(G, V), findall(arc(G, V, N, W), arc(G, V, N, W), Ns).



% adjs/3 true when V is a vertex in G and Vs is a list of all the adjacent
% vertices (in a non oriented graph interpretation)

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, N), arc(G, V, N, W), From),
    findall(vertex(G, N), arc(G, N, V, W), To),
    append(From, To, Vs).



% list_arcs/1 prints the list of all arcs in G

list_arcs(G) :- listing(arc(G, _, _, _)).



% list_graph/1 prints the list of all vertices and arcs

list_graph(G) :- list_arcs(G), list_vertices(G).
