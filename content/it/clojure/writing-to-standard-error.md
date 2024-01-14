---
title:                "Clojure: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error (stderr) è un'importante abilità nella programmazione di Clojure. Consente di mostrare messaggi di errore e informazioni di debug durante l'esecuzione del codice. 

## Come fare

Per scrivere a stderr in Clojure, è necessario utilizzare la funzione `println` con il parametro `System/err` come argomento. Ad esempio:

```Clojure
(println "Questo è un messaggio di errore" System/err)
```

L'output verrà stampato nel terminale, invece che sulla standard output (stdout).

## Approfondimento

In Clojure, stderr è gestito dal sistema di Java. Questo significa che è possibile utilizzare anche altre funzioni Java per scrivere a stderr, ad esempio `eprint` e `eprintln` dalla classe `java.lang.System`. Inoltre, è possibile utilizzare la global var `*err*` per accedere all'oggetto PrintWriter per scrivere a stderr.

## Vedi anche

- [Documentazione ufficiale di Clojure sulle funzioni System](https://clojure.org/reference/java_interop#_stdio)
- [Guida su come gestire gli errori in Clojure](https://www.clojure.org/guides/learn/exceptions)
- [Articolo sull'utilizzo delle funzioni Java in Clojure](https://medium.com/@jimfawcett/using-java-in-clojure-1455cd706111)