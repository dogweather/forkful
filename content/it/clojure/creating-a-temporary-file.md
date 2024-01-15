---
title:                "Creazione di un file temporaneo"
html_title:           "Clojure: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è una pratica comune nella programmazione, soprattutto quando si lavora con file o dati temporanei. Questo può essere utile per gestire i dati in modo più efficiente, evitare collisioni di nomi di file e facilitare la pulizia dei file temporanei dopo l'utilizzo.

## Come Fare

Per creare un file temporaneo in Clojure, è possibile utilizzare la funzione `with-open` in combinazione con la libreria `java.io.File`.

```
(ns temp-file-example
  (:require [clojure.java.io :as io]))

(with-open [f (io/file "/tmp/my-temp-file.txt")]
  (println "Ecco il mio file temporaneo:" f)
  (println "Il percorso è:" (.getPath f)))
```

L'output del codice precedente sarà simile a questo:

```
Ecco il mio file temporaneo: #object[java.io.File 0x3281df5a "/tmp/my-temp-file.txt"]
Il percorso è: /tmp/my-temp-file.txt
```

In questo esempio, il file temporaneo viene creato nella directory `/tmp` e viene assegnato alla variabile `f` all'interno del blocco di `with-open` per gestirne automaticamente la chiusura e la pulizia dopo l'utilizzo.

## Approfondimento

La funzione `with-open` utilizzata nell'esempio sopra accetta una lista di binding e un'espressione da valutare. Inoltre, è anche possibile utilizzare la funzione `io/delete-file` per eliminare il file temporaneo dopo l'utilizzo.

```
(with-open [f (io/file "/tmp/my-temp-file.txt")]
  (println "Ecco il mio file temporaneo:" f)
  (println "Il percorso è:" (.getPath f))
  (io/delete-file f)) ; Elimina il file temporaneo
```

Alcune possibili utilità dei file temporanei includono:

- Utilizzarli per scrivere i risultati di elaborazioni temporanee senza dover gestire manualmente la creazione e l'eliminazione dei file.
- Usarli come meccanismo di cache per dati che possono essere scompattati o ricostruiti in seguito, invece di creare file permanenti.
- Usarli per simulare l'accesso a file che non esistono realmente, quando si scrivono test automatizzati.

## Vedi Anche

- [Clojure Docs: gestione dei file](https://clojuredocs.org/clojure.java.io)
- [JavaDocs per `java.io.File`](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/io/File.html)
- [Tutorial introduttivo a Clojure](https://www.tutorialspoint.com/clojure/)