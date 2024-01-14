---
title:                "Clojure: In generazione di numeri casuali"
simple_title:         "In generazione di numeri casuali"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché Generare Numeri Casuali in Clojure?

Generare numeri casuali è un'attività comune nella programmazione, ed è particolarmente utile quando si vuole introdurre un elemento di aleatorietà o casualità in un programma. In Clojure, questo può essere fatto in modo semplice e veloce grazie alle funzionalità del linguaggio.

## Come Generare Numeri Casuali in Clojure

Per generare numeri casuali in Clojure, è necessario utilizzare la funzione `rand-int`. Questa funzione prende un parametro `n` e restituisce un numero intero casuale compreso tra 0 e `n - 1`. Ad esempio, per generare un numero casuale tra 0 e 9, si può utilizzare la seguente espressione:

```Clojure
(rand-int 10)
```

Esempio di output:

```Clojure
4
```

Per generare un numero casuale tra due valori specifici, ad esempio tra 1 e 100, si può utilizzare la funzione `+` per sommare il risultato di `rand-int` al valore iniziale. Ad esempio:

```Clojure
(+ 1 (rand-int 100))
```

Esempio di output:

```Clojure
57
```

## Approfondimento sulla Generazione di Numeri Casuali

La funzione `rand-int` è in realtà un'implementazione dell'algoritmo di generazione di numeri casuali Mersenne Twister, che è noto per essere veloce e molto efficiente. Clojure offre anche altre funzioni per generare numeri casuali, come `rand` che restituisce un numero decimale casuale tra 0 e 1. Inoltre, è possibile utilizzare la libreria `clojure.math.numeric-tower` per generare numeri casuali con distribuzioni diverse dalle uniformi, come ad esempio le distribuzioni normali o esponenziali.

## Vedi Anche

- [Documentazione di Clojure sulla generazione di numeri casuali] (https://clojure.org/api/java.util.Random)
- [Guida su come utilizzare la libreria clojure.math.numeric-tower] (https://clojuredocs.org/clojure.math.numeric-tower)