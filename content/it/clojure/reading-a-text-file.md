---
title:                "Lettura di un file di testo"
html_title:           "Clojure: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con linguaggi di programmazione funzionali o sei semplicemente curioso di esplorare nuovi paradigmi di programmazione, la lettura di un file di testo potrebbe risultare interessante. Inoltre, è una skill fondamentale per chi vuole automatizzare alcune attività legate alla gestione dei dati.

## Come fare

Per leggere un file di testo in Clojure, puoi utilizzare la funzione `slurp` che prende come argomento il percorso del file e restituisce una stringa con tutto il contenuto del file. Ad esempio:

```clojure
(def testo (slurp "testo.txt"))
```
La variabile `testo` conterrà il contenuto del file "testo.txt". Ora puoi utilizzarla per eseguire qualsiasi operazione su di esso.

## Approfondimento

In Clojure, è anche possibile utilizzare la libreria `clojure.java.io` per lavorare con i file. In particolare, la funzione `with-open` ti permette di leggere un file e chiuderlo automaticamente una volta finito, garantendo una gestione sicura delle risorse.

```clojure
(defn leggi-file [nome-file]
  (with-open [reader (clojure.java.io/reader nome-file)]
    (doseq [line (.readLines reader)]
      (println line))))
```
Il codice sopra utilizza la funzione `readLines` per leggere ogni riga del file e stamparla a schermo. Una volta terminato, il file viene chiuso automaticamente grazie alla funzione `with-open`.

## Vedi anche

- Clojure.io: https://clojure.org/reference/io
- Documentazione di Clojure.java.io: https://clojure.github.io/clojure/clojure.java.io-api.html
- Esempi di codice per leggere file in Clojure: https://gist.github.com/gregsexton/4445674