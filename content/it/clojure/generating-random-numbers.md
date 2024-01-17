---
title:                "Generazione di numeri casuali"
html_title:           "Clojure: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Generare numeri casuali è un processo che permette ai programmatori di generare numeri casuali in modo casuale, senza alcun ordine particolare. Questo è utile per creare giochi o applicativi dinamici, o per semplicemente aggiungere un elemento di casualità agli algoritmi.

## Come:
Generare numeri casuali in Clojure è molto semplice. Basta utilizzare la funzione ```rand```, e specificare il range all'interno del quale si desidera generare i numeri. Ad esempio:

```
(clojure.rand/rand 10) => 3  //genera un numero casuale tra 0 e 10
(clojure.rand/rand-nth [1 2 3 4 5]) => 4 //seleziona casualmente un elemento dall'array
```

## Approfondimento:
La generazione di numeri casuali è un processo utilizzato fin dall'inizio della programmazione. In precedenza, venivano utilizzati algoritmi elaborati per ottenere numeri che sembrassero casuali. Oggi, con l'avvento di linguaggi come Clojure, è possibile generare questi numeri in modo più semplice e veloce.
Alternative a ```rand``` includono la funzione ```rand-int``` che restituisce un intero tra due estremi, e la libreria ```math.combinatorics``` che permette la generazione di combinazioni casuali.
L'implementazione di ```rand``` in Clojure è basata sull'algoritmo di Java, che usa un generatore di numeri pseudo-casuali. Ciò significa che i numeri non sono veramente casuali, ma possono essere utilizzati a scopi pratici.

## Vedi anche:
- [Documentazione ufficiale sulla funzione rand](https://clojuredocs.org/clojure.core/rand)
- [Libreria math.combinatorics](https://github.com/clojure/math.combinatorics) per la generazione di combinazioni casuali
- [Articolo sulle alternative alla funzione rand](http://hypirion.com/musings/replacing-clojures-random-number-generator)