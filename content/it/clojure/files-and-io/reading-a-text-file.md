---
date: 2024-01-20 17:53:56.747636-07:00
description: "Leggere un file di testo consiste nell'acquisire dati da un file salvato\
  \ sul tuo disco. I programmatori lo fanno per elaborare o analizzare contenuti,\u2026"
lastmod: '2024-02-25T18:49:40.987877-07:00'
model: gpt-4-1106-preview
summary: "Leggere un file di testo consiste nell'acquisire dati da un file salvato\
  \ sul tuo disco. I programmatori lo fanno per elaborare o analizzare contenuti,\u2026"
title: Lettura di un file di testo
---

{{< edit_this_page >}}

## What & Why?
Leggere un file di testo consiste nell'acquisire dati da un file salvato sul tuo disco. I programmatori lo fanno per elaborare o analizzare contenuti, configurazioni, log e più.

## How to:
Clojure rende la lettura di file di testo semplice con poche righe di codice. Usiamo la funzione `slurp` per leggere tutto il contenuto del file o `line-seq` se vogliamo procedere riga per riga.

```Clojure
;; Uso di slurp per leggere l'intero contenuto di un file
(let [content (slurp "example.txt")]
  (println content))

;; Output (assumendo che il contenuto di example.txt sia "Ciao, mondo!")
; Ciao, mondo!

;; Uso di line-seq per gestire il file riga per riga
(with-open [reader (java.io.BufferedReader. (java.io.FileReader. "example.txt"))]
  (doseq [line (line-seq reader)]
    (println line)))

;; Output (per ogni riga di example.txt verrà stampata)
; Prima linea
; Seconda linea
; Terza linea
```

## Deep Dive
La funzione `slurp` è semplice e va benissimo per i file piccoli, ma non gestisce bene quelli grandi perché li legge tutti in memoria. Per i file più pesanti, è meglio usare `line-seq`, che lavora con le righe una per una. 

La lettura di file ha radici nei primi giorni dell'informatica, ma Clojure, essendo un linguaggio moderno, fornisce strumenti che si integrano bene con la piattaforma Java. Oltre a `slurp` e `line-seq`, ci sono librerie come `clojure.java.io` che offrono funzioni utility per lavorare con I/O.

Alternativamente, esistono librerie di terze parti più potenti e flessibili che si possono considerare se hai bisogno di più controllo o prestazioni, come `data.csv` per lavorare con file CSV o `clojure.data.json` per file JSON.

In Clojure, lavorare con file è spesso una questione di manipolazione di collezioni e laziness è una caratteristica chiave: piuttosto che leggere tutto un file in una volta, funzioni come `line-seq` leggono una riga alla volta, conforme la necessità.

## See Also
- Documentazione ufficiale di Clojure su I/O: https://clojure.github.io/clojure/clojure.java.io-api.html
- Clojure for the Brave and True, capitolo su I/O: https://www.braveclojure.com/io/
- Libreria clojure.data.csv: https://github.com/clojure/data.csv
- Libreria clojure.data.json: https://github.com/clojure/data.json
