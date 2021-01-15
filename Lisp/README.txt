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

    * L'API è in grado di memorizzare MST multipli se e solo se calcolati su
      grafi diversi. Una chiamata a mst-prim su un grafo eliminerà il
      risultato della precedente se calcolata sullo stesso, indipendentente
      dal vertice di partenza.

    * La capacità dello heap cresce all'inserimento se necessario.
      L'incremento della capacità è normalmente di 1 per garantire il minimo
      peso sulla memoria. Se si desidera aumentare la capacità manualmente
      è messa a disposizione la funzione 
      (heap-increase-capacity heap-id increment)

    * Per migliorare i tempi di ricerca all'interno dello heap è stata
      introdotta la hash-table *indices* contenente l'indici degli elementi
      all'interno dello array rappresente lo heap.

    * Non essendo necessaria, come da specifica, la funzione modify-key non è
      stata implementata.

    * La funzione new-arc crea un arco tra due vertici se e solo se questi
      appartengono al grafo specificato, in caso contrario viene generato
      l'errore "UNKNOWN VERTICES".
      Per scelta implementativa, il peso dell'arco non fa parte della sua
      chiave nella rispettiva hashtable, di conseguenza, creare un arco
      tramite la funzione new-arc quando già esistente ma con peso diverso,
      comporta la sostituzione di quello vecchio con quello appena creato.
      Dal momento che per rappresentare la relazione tra due vertici viene
      utilizzato un solo arco, dato un arco nella forma
      (ARC graph-id v-id u-id weight), creare tramite la funzione new-arc lo
      stesso arco ma passando i vertici in ordine opposto, comporta la
      sostituzione di quello già esistente con quello appena creato.

    * Viene fornita la funzione read-graph-from-csv per leggere grafi da
      file csv separati da tabulazioni. Questa funzione crea il grafo e i
      vertici se non già esistenti e gli archi che li collegano.


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
  impatto sulla memoria a scapito dei tempi di implementazione.

- Risulta stabile il tempo di esecuzione in ambienti cmucl e sbcl,
  indipendentemente dal numero di esecuzioni e senza alcun problema relativo
  alla gestione della memoria.

- Il codice allegato è stato indentato come richiesto da Emacs su 80 colonne.

- Allo scadere del termine ultimo per la consegna di febbrario, il repository
  GitHub in cui il progetto è stato sviluppato sarà reso pubblicamente
  disponibile al seguente link:
  https://github.com/LorenzoOlearo/minimum-spanning-trees/
