Minimum Spanning Trees
Progetto gennaio 2021 (E1P) Linguaggi di Programmazione Anno Accademico
2020-2021

- Componenti gruppo progetto:
  * Lorenzo Olearo, matricola ------
  * Alessandro Riva, matricola ------

- Implementazione Prolog dell'algoritmo di Prim per la risoluzione del
  problema MST.

- L'intera API, contenente l'implementazione di grafi, heaps e dell'algoritmo
  di Prim, descritta nella consegna è contenuta nel file "mst.pl".

- Caricamento della API da interprete Swipl:
  ?- consult('mst.pl').
  true.

Note riguardo l'implementazione:
- L'implementazione dei grafi ne supporta di orientati e non orientati con le
  uniche differenze nei predicati vertex_neighbors e adjs, dedicati
  all'interpretazione non orientata dei grafi, e vertex_neighbors_oriented e
  adjs_oriented, dedicati ai grafi orientati.
  Si è scelto di porre la differenza nella ricerca degli adiacenti e non
  nella creazione degli archi, dove si sarebbero potuti mettere due archi
  diretti a rappresentarne uno non diretto.
  Ciò implica inoltre che sia arc(graph, a, b, 1) nella base di conoscenza
  :- arc(graph, b, a, 1) fallirà ma potrà comparire come risultato dei
  predicati vertex_neighbors e adjs.

- I predicati new_arc/3 e new_arc/4 aggiungono un arco nella base di conoscenza
  se e solo se i vertici specificati appartengono al relativo grafo.
  Dato un arco nella forma arc(graph_id, u, v, weight) nella base di conoscenza,
  la creazione di un nuovo arco tra gli stessi vertici tramite il predicato
  new_arc, comporta la sostituzione dell'arco esistente con quello nuovo,
  indipendentente dal peso del collegamento.
  Dal momento che per rappresentare la relazione tra due vertici del grafo si è
  scelto di utilizzare un solo arco tra i due, dato un arco nella base di
  conoscenza arc(graph_id, u, v, weight), creare un arco tramite i predicati
  new_arc tra gli stessi vertici ma in ordine opposto, comporta la sostituzione
  del collegamento esistente con quello appena creato, indipendentente dal suo
  peso.

- Il predicato mst_prim è da utilizzare con grafi non orientati e non
  presenta il supporto ai grafi orientati.

- Ad ogni "consult" del file "mst.pl", tutti i predicati relativi al grafo ed
  il suo MST vengono rimossi dalla base di conoscenza.

- L'API è in grado di memorizzare mst multipli se e solo se calcolati su
  grafi diversi. Una chiamata a mst_prim su un grafo eliminerà il risultato
  della precedente, indipendentente dal vertice di partenza.

- Per consentire il caricamento di grafi tramite file csv sono forniti
  i predicati read_graph/2 e read_graph/3.
  Il primo è da usarsi per file separati da tabulazione
  Il secondo accetta come terzo argomento il codice ascii del carattere
  separatore.

Dettagli tecnici:
- La libreria fornita è stata sviluppata utilizzando Swipl, risulta
  correttamente caricata e funzionante sia su ArchLinux (SWI-Prolog version
  8.2.3 for x86_64-linux) che Windows 10 (SWI-Prolog version 8.2.1 for
  x64-win64).

- Il codice allegato è stato indentato come richiesto da Emacs su 80 colonne.

- Allo scadere del termine ultimo per la consegna di febbraio, il repository
  GitHub in cui il progetto è stato sviluppato sarà reso pubblicamente
  disponibile al seguente link:
  https://github.com/LorenzoOlearo/minimum-spanning-trees/

- La documentazione Prolog di ogni predicato è stata generata tramite PlDoc,
  non essendo consentita nella consegna, è disponibile nel repository GitHub
  sopracitato.
