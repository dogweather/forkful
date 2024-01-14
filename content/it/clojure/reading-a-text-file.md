---
title:                "Clojure: Leggere un file di testo"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

 Leggere un file di testo è un'operazione comune nella programmazione. È utile per estrarre dati da un file e utilizzarli in un programma o per effettuare operazioni specifiche sul contenuto del file. In questo articolo, impareremo come leggere un file di testo utilizzando Clojure.

## Come Fare

 In Clojure, possiamo utilizzare la funzione `slurp` per leggere il contenuto di un file di testo. Questa funzione accetta il percorso del file come argomento e restituisce una stringa con il contenuto del file. Ad esempio:

```Clojure
(def contenuto (slurp "file.txt"))
```

Per stampare il contenuto del file, possiamo utilizzare la funzione `println`:

```Clojure
(println contenuto)
```

Se il file contiene più righe, possiamo utilizzare la funzione `split-lines` per dividere il contenuto in una sequenza di stringhe, una per ogni riga:

```Clojure
(def righe (split-lines contenuto))
```

Possiamo quindi utilizzare una funzione `doseq` per iterare su ogni riga e stamparla a schermo:

```Clojure
(doseq [riga righe]
  (println riga))
```

## Approfondimento

 Esistono diverse librerie di terze parti che forniscono funzioni più avanzate per la lettura e la manipolazione di file di testo in Clojure, come ad esempio `clojure.java.io` e `clojure.data.csv`. Queste librerie offrono funzioni aggiuntive per specificare l'encoding del file da leggere, estrarre dati strutturati da file di testo e altro ancora.

## Vedi Anche

- [Documentazione di `clojure.java.io`](https://clojure.github.io/clojure/clojure.java.io-api.html)
- [Documentazione di `clojure.data.csv`](https://github.com/clojure/data.csv)