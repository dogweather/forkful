---
title:                "Trova la lunghezza di una stringa"
html_title:           "Clojure: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

 Trovare la lunghezza di una stringa è fondamentale per i programmatori di Clojure. Questa operazione ci permette di ottenere il numero di caratteri presenti in una stringa, che può essere utile in molte situazioni diverse.

## Come fare: 

Per trovare la lunghezza di una stringa in Clojure, possiamo utilizzare la funzione `count` che accetta come argomento una stringa e restituisce il numero di caratteri presenti. 
```Clojure
(count "ciao")
4
```

Possiamo anche utilizzare il metodo `.length` sulle stringhe che restituisce la lunghezza della stringa senza spazi. 
```Clojure
(.length "ciao")
4
```

## Approfondimento:

La funzione `count` è stata introdotta già nella prima versione di Clojure e si basa sul concetto di "seq", dove una stringa è trattata come una sequenza di caratteri. 
Un'alternativa alla funzione `count` è l'utilizzo del metodo `str-length` che può essere più efficiente in alcune situazioni in quanto non richiede la creazione di una sequenza temporanea. 
Inoltre, possiamo trovare la lunghezza di una sequenza utilizzando la funzione `clojure.core/long` che accetta come argomento una collezione qualunque e ne restituisce la lunghezza in formato numerico.

## Vedi anche:

- Documentazione ufficiale di Clojure sulla funzione `count`: https://clojuredocs.org/clojure.core/count
- Altro metodo utile per trovare la lunghezza di una stringa con Clojure: https://techwayahead.com/clojure/2017/04/23/count-vs-length-in-clojure.html