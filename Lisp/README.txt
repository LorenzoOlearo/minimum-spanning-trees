Implementazione Common Lisp dell'algoritmo di Prim per la risoluzione del
problema MST.

- Componenti gruppo progetto:
  * Lorenzo Olearo, matricola ------
  * Alessandro Riva, matricola ------

- L'intera API, contenente l'implementazione di grafi, heaps e dell'algoritmo
  di Prim, descritta nella consegna è contenuta nel file "mst.lisp".

- Caricamento della API da interprete:
  CL-USER> (load "path/to/mst.lisp")

Note riguardo l'implementazione:
- L'API supporta esclusivamente e grafi non orientati.
  Si è scelto di rappresentarli con un solo arco per indicare entrambe
  le direzioni.

- L'API è in grado di memorizzare mst multipli se e solo se calcolati su
  grafi diversi. Una chiamata a mst-prim su un grafo eliminerà il risultato
  della precedente, indipendentente dal vertice di partenza.

- Per migliorare i tempi di ricerca all'interno dello heap è stata introdotta
  la hash-table *indices* contenente l'indice degli elementi all'interno dello
  array rappresente lo heap

- Si è scelto di implementare lo heap in modo da permettere la ricerca
  su chiavi univoche diverse dalle chiavi di ordinamento.
  Si è introdotto quindi la funzione heap-insert-extended che pone il primo
  elemento, della lista passata come valore, o l'elemento stesso se un atomo
  da insere a chiave univoca
  Utilizzando heap-insert, tutto il valore in input, sia esso lista o atomo
  è posto a chiave univoca
  Se ne illustra qui il funzionamento
  assumendo value, value-1 e value-2 atomici

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


  Questa strutturazione dello heap è introdotta per i notevoli vantaggi
  che porta ai tempi di esecuzione dell'algoritmo di Prim. Tuttavia, questo
  comporta un maggiore uso della memoria che può portare al superamento
  del limite di memoria imposto dall'ambiente lispworks personal edition.
  Al fine di fornire un maggior supporto è presentata anche la versione
  della libreria mst-low-mem.lisp, che riduce l'uso di memoria a scapito
  dei tempi di esecuzione.


Dettagli tecnici:
- La libreria fornita risulta correttamente caricata ed eseguita nella sua
  interezza dai seguenti interpreti:
      * Lispworks Personal Edition 7.1 (Windows 10 e ArchLinux)
      * cmucl (ArchLinux)
      * sbcl (Windows 10 e ArchLinux)

- Per via del funzionamento del garbage collector l'implementazione in
  mst.lisp in un contesto a molti archi (>200k archi) risulta richiedere
  più risorse di quelle consentite dall'ambiente lispworks personal
  edition.
  Si riscontrano inoltre fallimenti per esecuzioni ripetute dell'algoritmo
  di Prim. Si suppone che il garbage collector non liberi la memoria degli
  array dereferenziati, portando al superamento del limite di memoria
  imposto da lispworks personal edition.
  Per un maggior supporto nei casi di raggiungiumento del limite della
  memoria si presenta anche la libreria mst-low-mem.lisp, che ha un minor
  impatto sulla memoria a scapito dei tempi di implementazione

- Risulta inoltre stabile il tempo di esecuzione in ambiente cmucl e sbcl,
  indipendentemente dal numero di esecuzione.

- Il codice allegato è stato indentato come richiesto da Emacs su 80 colonne.

- Allo scadere del termine ultimo per la consegna, il repository GitHub in cui
  il progetto è stato sviluppato sarà reso pubblicamente disponibile al
  seguente link: https://github.com/LorenzoOlearo/minimum-spanning-trees/
