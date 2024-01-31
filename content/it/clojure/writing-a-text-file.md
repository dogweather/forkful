---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare dati in formato leggibile. Programmatori lo fanno per persistenza dei dati, configurazioni, o esportazione di risultati.

## How to:
```Clojure
; Creare un file di testo e scrivere una stringa
(spit "example.txt" "Ciao, questo è testo in un file.")

; Aggiungere più testo al file esistente
(spit "example.txt" " Ecco un'altra riga." :append true)
```
Resultato nel file `example.txt`:
```
Ciao, questo è testo in un file. Ecco un'altra riga.
```

## Deep Dive
La funzione `spit` usata qui è concisa e comoda per la scrittura di file, rispetto a metodi più verbosi in altri linguaggi. Storicamente, linguaggi come C richiedono diverse chiamate di sistema per ottenere lo stesso risultato. Clojure, sviluppando su Java, fornisce un alto livello di astrazione. Alternative includono l'uso di `java.io.Writer` per controllo più fine.

## See Also
- [Clojure Documentation on I/O](https://clojure.github.io/clojure/clojure.java.io-api.html)
