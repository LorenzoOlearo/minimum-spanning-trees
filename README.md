# Minimum Spanning Trees
### January 2021 Project (E1P), Programming Languages Academic Year 2020-2021

  * [Common Lisp](#common-lisp)
    + [Loading](#loading)
    + [Features](#features)
      - [Multiple MSTs](#multiple-msts)
      - [Speed optimized](#speed-optimized)
      - [Read graphs from csv files](#read-graphs-from-csv-files)
    + [Implementation notes](#implementation-notes)
      - [Graphs](#graphs)
      - [Indices](#indices)
      - [Modify Key](#modify-key)
      - [Extended functions](#extended-functions)
    + [Demo](#demo)

* [Credits](#credits)
* [Bibliography](#bibliography)



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

#### Modify Key
As of specific and as not needed, the function modify-key is not implemented,
only the function `DECREASE-KEY` is used.

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

