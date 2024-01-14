---
title:                "Clojure: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Lettura di un file di testo può essere molto utile per gli sviluppatori Clojure. Ci permette di accedere alle informazioni memorizzate in un file e di utilizzarle nel nostro codice. In questo articolo, esploreremo come leggere un file di testo utilizzando Clojure.

## Come fare

Per leggere un file di testo in Clojure, dobbiamo prima importare il modulo `clojure.java.io`. Questo modulo ci fornisce tutte le funzioni necessarie per lavorare con file di testo.

```clojure
(require '[clojure.java.io :as io])
```

Una volta importato il modulo, possiamo utilizzare la funzione `slurp` per leggere il contenuto di un file di testo. Definiamo una variabile `file` contenente il percorso del file che vogliamo leggere.

```clojure
(def file "file.txt")
```

Ora possiamo utilizzare la funzione `slurp` passando il percorso del file come parametro e assegnare il contenuto a una variabile `content`.

```clojure
(def content (slurp file))
```

Possiamo quindi stampare il contenuto del file utilizzando la funzione `println`.

```clojure
(println content)
```

Il risultato dovrebbe essere il seguente:

```
Questo è il contenuto del mio file di testo.
```

## Approfondimento

Oltre alla funzione `slurp`, il modulo `clojure.java.io` ci fornisce anche altre funzioni utili per lavorare con file di testo. Ad esempio, possiamo utilizzare la funzione `file-seq` per ottenere un'elenco di tutti i file in una determinata directory e la funzione `file?` per verificare se un determinato percorso si riferisce a un file o a una directory.

Inoltre, Clojure ci offre la possibilità di leggere e scrivere file in formato JSON utilizzando il modulo `clojure.data.json`.

## Vedi anche

- [Documentazione di clojure.java.io](https://clojuredocs.org/clojure.java.io)
- [Documentazione di clojure.data.json](https://github.com/clojure/data.json)