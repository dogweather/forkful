---
date: 2024-01-20 17:52:14.983534-07:00
description: "Stampare output di debug significa scrivere informazioni a schermo per\
  \ capire cosa sta succedendo nel tuo codice. I programmatori lo fanno per trovare\
  \ e\u2026"
lastmod: '2024-03-13T22:44:43.044737-06:00'
model: gpt-4-1106-preview
summary: Stampare output di debug significa scrivere informazioni a schermo per capire
  cosa sta succedendo nel tuo codice.
title: Stampa dell'output di debug
weight: 33
---

## What & Why?
Stampare output di debug significa scrivere informazioni a schermo per capire cosa sta succedendo nel tuo codice. I programmatori lo fanno per trovare e risolvere errori velocemente.

## How to:
Clojure rende facile la stampa per il debug. Usa `println` per vedere il valore delle variabili, o `prn` per avere una versione più "raw" della stampa.

```Clojure
;; Stampa normale
(println "Debugging valore:" (+ 1 2 3))

;; Output:
;; Debugging valore: 6

;; Stampa raw
(prn "Debugging valore:" (+ 1 2 3))

;; Output:
;; "Debugging valore:" 6
```

`println` e `prn` possono aiutare a vedere cosa sta succedendo, soprattutto quando ci si aspetta qualcosa di diverso.

## Deep Dive
La stampa per il debug in Clojure non è diversa da altri linguaggi. Iniziata negli anni '70 come un metodo semplice e diretto per controllare lo stato di un'applicazione, è ancora utile. Nonostante ci siano strumenti avanzati come debugger o loggers, la stampa resta un metodo veloce e pratico quando in fase di sviluppo.

Alternative includono l'utilizzo di strumenti di logging configurabili, che sono più potenti ma richiedono setup iniziale. In Clojure abbiamo ad esempio `tools.logging` o librerie di terze parti come `log4j`.

Riguardo l'implementazione, `println` scrive sullo standard output, mentre `prn` aggiunge anche le virgolette ai valori stringa e stampa la rappresentazione esatta di altri dati. Questo può essere fondamentale per distinguere tra tipi differenti di dati.

## See Also
- [Clojure Documentation](https://clojure.org/)
- [clojure.tools.logging](https://github.com/clojure/tools.logging)
- [Log4j](https://logging.apache.org/log4j/2.x/)
