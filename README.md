# Minimum Spanning Trees - *heap heap hooray*
### January 2021 Project (E1P), Programming Languages Academic Year 2020-2021

The project is divided into two implementation of the same Prim's algorithm, one
in Common Lisp while the other in Prolog.
  
- [Common Lisp](#common-lisp)
  * [Loading](#loading)
  * [Features](#features)
    + [Multiple MSTs](#multiple-msts)
    + [Speed optimized](#speed-optimized)
    + [Read graphs from csv files](#read-graphs-from-csv-files)
  * [Implementation notes](#implementation-notes)
    + [Graphs](#graphs)
    + [Indices](#indices)
    + [Modify key](#modify-key)
    + [New arcs](#new-arcs)
    + [Extended functions](#extended-functions)
  * [Demo](#demo)
- [Prolog](#prolog)
  * [Loading](#loading-1)
  * [Documentation (`pldoc`)](#documentation---pldoc--)
  * [Implementation notes](#implementation-notes-1)
    + [Multiple MSTs](#multiple-msts-1)
    + [Arcs representation](#arcs-representation)
    + [mst_prim](#mst-prim)
    + [Consult](#consult)
    + [Read graphs from CSV files](#read-graphs-from-csv-files)
  * [Demo](#demo-1)


## Common Lisp 
### Loading
The entire API can be loaded from REPL with

`CL-USER> (load "path/to/mst.lisp")`

The API is tested and correctly executed with the following interpreters:

  * Lispworks Personal Edition 7.1 (ArchLinux and Windows 10)
  * `cmucl 21d-1` (ArchLinux)
  * `sbcl 2.1.1` (ArchLinux and Windows 10)


### Features 

#### Multiple MSTs
The API is capable of keeping in memory multiples MST as long as the are 
computed on graphs with differents IDs.

#### Speed optimized
Fast vertex neighbors search and access directly in the heap.

#### Read graphs from csv files
The function read-graph-from-csv allow loading all the needed structures for the
graph representation from a csv file. The function will create all necessary
vertices and the respective graph, the graph's arcs in the CSV file must have
the following notation:

``` csv
source    destination    weight
```

All arcs are then stored into the `*arcs*` hashtable in the form

``` common-lisp
(arc graph-id source-id dest-id weight)
```


### Implementation notes

#### Graphs
The API only supports non-oriented graphs, the relationship between two nodes is
represented with a sole arc instead of one for each direction for memory
optimization purposes.

#### Indices
A hash-table `*indices*` is used to store the indices of the elements contained
in the array representing the min-heap.

#### Modify key
As of specific and as not needed, the function modify-key is not implemented,
only the function `DECREASE-KEY` is used.

#### New arcs
The function `NEW-ARC` creates an arc between two vertices if and only if these
are part of the function specified graph.
The arc's weight it's not part of its hashtable key, hence, creating an arc
using the function `NEW-ARC` when already present but with different wieght will
replace the existing one with the newly created.
Due to the fact that to represent the relation between two vertices only one arc
with the form `(ARC graph-id v-id u-id weight)` it's used, creating the same arc
with `NEW-ARC` but while inverting `v-id` and `u-id` will replace the existing
arc with the newly created one.

#### Extended functions
The heap implementation allow searching on unique keys different from the order
keys through function `HASHED-HEAP-FIRST-INDEX`. 
Therefore, the it's introduced the function `HEAP-INSERT-EXTENDED` that sets its
first element from the list passed as an argument, or the same element if atomic
and thus sets with unique key.

This behavior is illustrated below in more detail assuming `value`, `value-1`,
`value-2` atomics.

``` common-lisp

(heap-insert heap-id key value)
(heap-extract heap-id) --> (key value)

(heap-insert heap-id key value)
(heap-extract-extended heap-id) --> (key (value))

(heap-insert-extended heap-id key value)
(heap-extract heap-id) --> (key value)

(heap-insert-extended heap-id key value)
(heap-extract-extended heap-id) --> (key (value))

(heap-insert heap-id key (list value-1 value-2))
(heap-extract heap-id) --> (key (value-1 value-2))

(heap-insert heap-id key (list value-1 value-2))
(heap-extract-extended heap-id) --> (key ((value-1 value-2)))

(heap-insert-extended heap-id key (list value-1 value-2))
(heap-extract heap-id) --> (key value-1)

(heap-insert-extended heap-id key (list value-1 value-2))
(heap-extract heap-id) --> (key (value-1 value-2))

```

The function `HEAP-HEAD-EXTENDED` behavior is analogue to
`HEAP-EXTRACT-EXTENDED` with difference being the fact that the first will also
extract the first element from the heap.

This heap structure is introduced for the considerable improvements in term of
execution time of the Prim's algorithm, however result the increase of the
required memory that may exceed the limits of *Lispworks Personal Edition*. In
order to provide additional support, the branch `mst-low-mem` uses a more
conventional heap structure.


### Demo
In order to stress test the API, under `/Lisp/benchmarks` are provided three
graphs in CSV notation each with 10K, 50K and 500K arcs. In order to load these
graphs, loading first `demo.lisp` is required.



## Prolog

### Loading
The entire API can be loaded from `swipl` with

``` prolog
?- consult('mst.pl').

true.
```

The API is tested and correctly executed with `swipl 8.2.3` on ArchLinux and
`swipl 8.2.1` on Windows 10

### Documentation (`pldoc`)
All the comments in the code are written following the swipl's `pldoc`
specification, the `pldoc` documentation of the prolog project can be generated
with the query 

`:- doc_save('mst.pl', [doc_root('./doc')])`


### Implementation notes

#### Multiple MSTs
The API is capable of keeping in memory multiples MST as long as the are 
computed on graphs with differents IDs. A query with the predicate `mst_prim`
will retract the result of the previous one if computed on the same graph,
indepently from the source vertex.

#### Arcs representation
The implementation supports both oriented and non-oriented graphs with the only
difference being in the predicate `vertex_neighbors` and `adjs` aimed to
non-oriented graphs. The corresponding *oriented* predicates
`vertex_neighbors_oriented` and `adj_oriented` are provided. It was decided to
make the difference in the search for adjacent and non adjacent vertices instead
of using two arcs between two vertices to represent a non-direct one. This
implies that if the arc `(graph, a, b, 1)` is in the knowledge base, the query
`:- (graph, b, a, 1)` will fail while appearing as results of the predicates
`vertex_neighbors` and `adjs`.

#### mst_prim
The predicate `mst_prim` is intented to be used with non-oriented graphs and
does not support oriented ones.


#### Consult
At each `consult` of `mst.pl`, each and every predicate in the knowledge base
will be removed.

#### Read graphs from CSV files
In order to allow loading graphs from CSV files, the predicate `read_graphs/2`
and `read_graph/3` are provided. The first is intended to be used with tab
separated files with the other allow for the ASCII separator to be specified as
an argument.


### Demo
In order to stress test the API, under `/prolog/benchmarks` are provided three
graphs in CSV notation each with 10K, 50K and 500K arcs. 
