Minimum Spanning Trees
Progetto gennaio 2021 (E1P) Linguaggi di Programmazione Anno Accademico
2020-2021

- Componenti gruppo progetto:
  * Lorenzo Olearo, matricola ------
  * Alessandro Riva, matricola ------

- Implementazione Common Lisp dell'algoritmo di Prim per la risoluzione del
  problema MST.

- L'intera API, contenente l'implementazione di grafi, heaps e dell'algoritmo
  di Prim, descritta nella consegna è contenuta nel file "mst.lisp".

- Caricamento della API da interprete:
  CL-USER> (load "path/to/mst.lisp")

- Note riguardo l'implementazione:
    * L'API supporta esclusivamente grafi non orientati.
      Si è scelto di rappresentarli con un solo arco per indicare entrambe
      le direzioni.

    * L'API è in grado di memorizzare mst multipli se e solo se calcolati su
      grafi diversi. Una chiamata a mst-prim su un grafo eliminerà il
      risultato della precedente, indipendentente dal vertice di partenza.

    * Per migliorare i tempi di ricerca all'interno dello heap è stata
      introdotta la hash-table *indices* contenente l'indice degli elementi
      all'interno dello array rappresente lo heap.

    * Non essendo necessaria, come da specifica, la funzione modify-key non è
      stata implementata.

    * La funzione new-arc crea l'arco se e solo se il grafo e i vertici 
      specificati sono già esistenti, e i vertici fanno parte del grafo.
      In caso contrario viene generato l'errore "UNKNOWN VERTICES".

    * Viene fornita la funzione read-graph-from-csv per leggere grafi da
      file csv separati da tabulazioni. Questa funzione crea il grafo e i 
      vertici se non già esistenti e gli archi che li collegano.

    * Si è scelto di implementare lo heap in modo da permettere la ricerca
      su chiavi univoche diverse dalle chiavi di ordinamento tramite 
      la funzione hashed-heap-first-index.
      Si è introdotta quindi la funzione heap-insert-extended che pone il
      primo elemento, della lista passata come valore, o l'elemento stesso se
      un atomo da insere a chiave univoca. Utilizzando heap-insert, tutto il
      valore in input, sia esso lista o atomo, è posto a chiave univoca.
      Se ne illustra qui il funzionamento, assumendo value, value-1 e value-2
      atomici.

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

      La funzione heap-head-extended funziona analogamente a 
      heap-extract-extended con la differenza che la prima non estrae il
      valore dallo heap.

      Questa strutturazione dello heap è introdotta per i notevoli vantaggi
      che porta ai tempi di esecuzione dell'algoritmo di Prim, introduce
      tuttavia un maggiore uso della memoria che può portare al superamento
      del limite di memoria imposto dall'ambiente Lispworks Personal Edition.
      Al fine di fornire un maggior supporto, è presentata anche la versione
      della libreria "mst-low-mem.lisp", che non utilizza tale struttura per
      lo heap riducendo così l'uso di memoria a scapito dei tempi di
      esecuzione. In tale versione la ricerca è effettuata su tutto il valore
      dell'elemento nello heap tramite la funzione heap-first-index, è
      quindi assente la differenza sopracitata tra chiave di ricerca e 
      valore. 


Dettagli tecnici:
- La libreria fornita risulta correttamente caricata ed eseguita nella sua
  interezza dai seguenti interpreti:
    * Lispworks Personal Edition 7.1 (Windows 10 e ArchLinux)
    * cmucl (ArchLinux)
    * sbcl (Windows 10 e ArchLinux)

- Per via del funzionamento del garbage collector l'implementazione in
  mst.lisp in un contesto a molti archi (>200k archi) risulta richiedere
  più risorse di quelle consentite dall'ambiente Lispworks Personal
  Edition.
  Sempre e soltanto nell'ambiente Lispworks Personal Edition, si possono
  verificare fallimenti per esecuzioni ripetute dell'algoritmo di Prim. Si
  suppone che il garbage collector non liberi la memoria degli array
  dereferenziati, portando al superamento del limite di memoria imposto da
  Lispworks Personal Edition.
  Per un maggior supporto nei casi di raggiungiumento del limite della
  memoria si presenta anche la libreria "mst-low-mem.lisp", che ha un minor
  impatto sulla memoria a scapito dei tempi di esecuzione.

- Risulta stabile il tempo di esecuzione in ambienti cmucl e sbcl,
  indipendentemente dal numero di esecuzioni e senza alcun problema relativo
  alla gestione della memoria.

- Il codice allegato è stato indentato come richiesto da Emacs su 80 colonne.

- Allo scadere del termine ultimo per la consegna, il repository GitHub in cui
  il progetto è stato sviluppato sarà reso pubblicamente disponibile al
  seguente link: https://github.com/LorenzoOlearo/minimum-spanning-trees/
