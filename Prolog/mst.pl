%%%% -*- Mode: Prolog -*-

% Do not autoload from autoload libraries
% autoload(explicit).


% Load the Swipl's library that parses and generates CSV data
use_module(library(csv)).



% Informs the interpreter that the definition of the predicates may change
% during execution (using assert/1 and/or retract/1).



% new_graph/1 Adds a graph to the knowledge base if not already existing.

new_graph(G) :- graph(G), !.

new_graph(G) :- assert(graph(G)), !.


% delete_graph/1 Removes a graph and all his nodes and arches form the knowledge
% base

delete_graph(G) :-
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(graph(G)).



% new_vertex/2 Adds a vertex V to a graph in the knowledge base. false if the
% graph is not in the knowledge base

new_vertex(G, V) :-
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)), !.



% vertices/2 True when Vs is a list of every vertex in G

%vertices(G, Vs) :- findall(V, vertex(G, V), Vs).
vertices(G, Vs) :- findall(vertex(G, V), vertex(G, V), Vs).



% list_vertices/1 Prints the list of vertices in a graph G

list_vertices(G) :- listing(vertex(G, _)).



% new_arc/4 adds a weighted arc (an edge according to graph theory) to a graph G
% in the knowledge base

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, U, V, Weight), !.

new_arc(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(arc(G, U, V, Weight)), !.


% new_arc/3 adds a weighted arc with weight 1 (an edge according to graph
% theory) to a graph G in the knowledge base

new_arc(G, U, V) :- new_arc(G, U, V, 1).



% arcs/2 true when Es is a list of every arcs in a graph G

arcs(G, Es) :-
    graph(G),
    findall(arc(G, V, U, W), arc(G, V, U, W), Es).



% neighbors/3 true when V is a vertex in G and Ns is a list of all the arcs
% from V

neighbors(G, V, Ns) :-
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), Ns).



% adjs/3 true when V is a vertex in G and Vs is a list of all the adjacent
% vertices (in a non oriented graph interpretation)

adjs(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, N), arc(G, V, N, W), From),
    findall(vertex(G, N), arc(G, N, V, W), To),
    append(From, To, Vs).



% adjs_oriented/3 true when V is a vertex in G and Vs is a list of all the 
% adjacent vertices (in a oriented graph interpretation)

adjs_oriented(G, V, Vs) :-
    vertex(G, V),
    findall(vertex(G, N), arc(G, V, N, _), Vs).



% list_arcs/1 prints the list of all arcs in G

list_arcs(G) :- listing(arc(G, _, _, _)).



% list_graph/1 prints the list of all vertices and arcs

list_graph(G) :-
    list_arcs(G),
    list_vertices(G).



% read_graph/2 creates a new graph G and its arcs and verices according
% to csv file FileName with tabulation as separator

read_graph(G, FileName) :-
    csv_read_file(FileName, Rows, [separator(0'\t)]),
    new_graph_from_rows(G, Rows).



% new_graph_from_rows/2 support procedure to create a graph G given a list in
% the format[row(V, U, W)] where V is source vertex, U is destination vertex
% and W is weight of the arc between the two

new_graph_from_rows(G, []) :- new_graph(G), !.

new_graph_from_rows(G, [Row | Rows]) :-
    Row =.. [row, V, U, W],
    new_graph(G),
    new_vertex(G, V),
    new_vertex(G, U),
    new_arc(G, V, U, W),
    new_graph_from_rows(G, Rows).


% write_graph/3 writes a graph G along with its arcs and vertices to a CSV file
% represented by FileName

write_graph(G, FileName, graph) :-
    arcs(G, Arcs),
    write_arcs_in_rows(Arcs, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).

write_graph(G, FileName, edges) :-
    arcs(G, Arcs),
    write_arcs_in_rows(Arcs, Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]).

write_graph(G, FileName) :- write_graph(G, FileName, graph).



% write_graph_in_rows/2 support predicate for write_graph/2, given a list with
% the all the arcs of the graph it creates a formatted list where each entry is
% the predicate row/3 and its terms are in the order: the source vertex V, the
% destination vertex U and the weight of the arc between the two

write_arcs_in_rows([], []) :- !.

write_arcs_in_rows([Arc | Arcs], [Row | Rows]) :-
    Arc =.. [_, _ | TRow],
    Row =.. [row | TRow],
    write_arcs_in_rows(Arcs, Rows).
