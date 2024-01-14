---
title:                "Clojure: Scrivere un file di testo"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Perché
Scrivere un file di testo è una delle basi della programmazione. Ti permette di creare e manipolare dati, scrittura di report e altro ancora.

# Come fare
Per scrivere un file di testo in Clojure, segui questi 4 semplici passi:
1. Importa la libreria `clojure.java.io` per accedere alle funzioni di IO.
2. Apri un file utilizzando la funzione `clojure.java.io/writer` e specifica il percorso in cui desideri scrivere il file.
3. Utilizza la funzione `clojure.java.io/write` per scrivere i dati nel file, passando come argomento il file aperto e i dati da scrivere.
4. Chiudi il file utilizzando la funzione `close`.

Ecco un esempio di codice per scrivere i numeri da 1 a 10 in un file di testo chiamato "numeri.txt":

```Clojure
(ns file-testo
 (:require [clojure.java.io :as io]))

(defn scrivi-numeri []
  (let [file (io/writer "numeri.txt")]
    (dotimes [num 10]
      (io/write file (str num " ")))
    (io/close file)))

(scrivi-numeri)
```

L'output nel file "numeri.txt" sarà: `0 1 2 3 4 5 6 7 8 9`. Puoi anche scrivere stringhe o dati in formato CSV utilizzando la stessa logica.

# Approfondimento
Oltre a scrivere dati in semplici file di testo, puoi utilizzare la libreria `clojure.data.csv` per scrivere in file CSV formattati in modo strutturato. Inoltre, puoi manipolare o filtrare i dati prima di scriverli nel file utilizzando funzioni di manipolazione dei dati come `map`, `filter`, `reduce` e altro ancora.

# Vedi anche
- [clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [clojure.data.csv](https://github.com/clojure/data.csv)