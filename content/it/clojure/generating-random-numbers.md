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

## Perché

Generare numeri casuali è un'attività molto comune nella programmazione. Questo può essere utile per testare le funzioni, creare giochi o quiz, o semplicemente per divertimento.

## Come fare

Per generare un numero casuale in Clojure, puoi utilizzare la funzione predefinita "rand-int". Ad esempio:

```Clojure
(rand-int 10)
```

In questo caso, verrà generato un numero intero casuale compreso tra 0 e 10 (escluso). Per includere 10 nel range, è possibile utilizzare la funzione "rand-nth" con una sequenza che contiene il valore desiderato. Ad esempio:

```Clojure
(rand-nth [1 2 3 4 5 6 7 8 9 10])
```

Ci sono anche altre funzioni disponibili per generare numeri casuali in Clojure, come ad esempio "rand", "rand-float" e "rand-nth". Ecco un esempio di come utilizzare "rand-float" per generare un numero casuale con due cifre decimali:

```Clojure
(rand-float 100)
```
Questo genererà un numero con due cifre decimali compreso tra 0 e 100 (escluso).

## Approfondimento

Molte volte, la generazione di numeri casuali richiede anche il controllo di alcune condizioni, come ad esempio l'esclusione di alcune cifre o la generazione di valori unici. In Clojure, puoi utilizzare la libreria "clojure.set" per eseguire queste operazioni in modo efficiente.

Ad esempio, se vuoi generare 5 numeri casuali compresi tra 1 e 10 (esclusi), puoi utilizzare la funzione "clojure.set/shuffle" per mescolare una sequenza di numeri compresi tra 1 e 10 e poi utilizzare la funzione "take" per estrarre solo i primi 5 numeri dalla sequenza mescolata. Ecco un esempio di come farlo:

```Clojure
(require '[clojure.set :as set])

(def numeri (range 1 10))
(def numeri-shuffle (set/shuffle numeri))
(take 5 numeri-shuffle)
```

Questo genererà una sequenza con 5 numeri casuali che puoi utilizzare a tuo piacimento.

## Vedi anche

- [Documentazione ufficiale di Clojure](https://clojure.org/)
- [Clojure Cookbook](https://www.clojure-cookbook.com/)
- [The Joy of Clojure](https://www.manning.com/books/the-joy-of-clojure)