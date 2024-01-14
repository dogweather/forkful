---
title:    "Clojure: Scrivere su errore standard"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error può essere utile per visualizzare informazioni di debug durante l'esecuzione di un programma Clojure. Invece di stampare tutto sullo standard output, possiamo inviarlo a standard error per mantenerlo separato e più facile da analizzare.

## Come Fare

Per scrivere su standard error in Clojure, possiamo utilizzare la funzione `println` insieme alla macro `System/err`. Ecco un esempio di codice:

```Clojure
(ns blog-post.core
  (:import (java.lang System)))
  
(println "Questo viene stampato su standard output")

(System/err println "Questo viene stampato su standard error")
```

L'output di questo esempio sarà il seguente:

```
Questo viene stampato su standard output
Questo viene stampato su standard error
```

## Approfondimento

Scrivere su standard error è particolarmente utile quando stiamo sviluppando un programma che utilizza lo standard output per mostrare informazioni al nostro utente. In questo modo, possiamo distinguere facilmente i messaggi di debug dal normale output del programma.

Inoltre, quando stiamo scrivendo un programma che deve essere eseguito in background o in uno script, gli errori e i messaggi di debug su standard error sono più facili da catturare e analizzare.

## Vedi Anche

- Documentazione ufficiale di Clojure su `println`: https://clojuredocs.org/clojure.core/println
- Esempi avanzati di utilizzo di `System/err`: https://clojuredocs.org/clojure.java.io/system/err