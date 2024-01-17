---
title:                "Stampa degli output di debug"
html_title:           "Clojure: Stampa degli output di debug"
simple_title:         "Stampa degli output di debug"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e perché: 
Il printing debug output è una tecnica utilizzata dai programmatori per visualizzare informazioni durante l'esecuzione del codice, al fine di identificare errori e comprendere i processi interni del programma.

## Come fare:
Ecco un esempio di come stampare debug output in Clojure:

```Clojure
(defn calc-sum [a b]
  (println "Inizio calcolo somma")
  (let [sum (+ a b)]
    (println "Fine calcolo somma")
    sum))

(calc-sum 3 5)
```
Output:
```
Inizio calcolo somma
Fine calcolo somma
8
```
## Approfondimento:
Il printing debug output è una tecnica popolare fin dalle prime fasi della programmazione. In passato, i programmatori utilizzavano strumenti come la stampa su carta o il debug visuale per visualizzare informazioni di debug. Oggi, con l'avanzamento della tecnologia, è possibile utilizzare potenti strumenti di debug integrati nei moderni ambienti di sviluppo.

## Vedi anche:
- [Guida di Clojure al debugging](https://clojure.org/guides/debugging)
- [Come stampare debug output in altri linguaggi di programmazione](https://www.geeksforgeeks.org/debugging-in-python-how-to-get-abstract-dynamic-and-configurable-output/)