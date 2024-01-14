---
title:                "Clojure: Generazione di numeri casuali"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generating random numbers è una funzionalità essenziale nella programmazione. Può essere utilizzata per testare il codice o creare dati casuali per applicazioni come giochi o simulazioni.

## Come Fare

Per generare numeri casuali in Clojure, possiamo utilizzare la funzione `rand` della libreria `clojure.core`.

```
Clojure
(let [random-num (rand)]
  (println random-num))
```

Questo codice stampa un numero casuale tra 0 e 1. Possiamo anche fornire dei limiti come argomenti alla funzione `rand`, per esempio per generare un intero casuale tra 1 e 10:

```
Clojure
(let [random-num (rand-int 10)]
  (println random-num))
```

Possiamo anche utilizzare la funzione `rand-nth` per selezionare un elemento casuale da una sequenza:

```
Clojure
(let [numbers [1 2 3 4 5]]
  (println (rand-nth numbers)))
```

## Approfondimento

La funzione `rand` utilizza l'algoritmo Park-Miller per generare numeri pseudo-casuali. Questo significa che i numeri generati possono sembrare casuali, ma in realtà seguono un pattern prevedibile. Per generare numeri veramente casuali, possiamo utilizzare la libreria di terze parti `alea`, che implementa l'algoritmo Mersenne Twister.

Ecco un esempio di utilizzo della libreria `alea` per generare 10 numeri casuali tra 1 e 100:

```
Clojure
(require '[alea.core :refer [mt-random]])
(let [random-numbers (repeatedly 10 #(mt-random 1 100))]
  (println random-numbers))
```

## Vedi Anche

- [Documentazione di Clojure sulle funzioni `rand`, `rand-int` e `rand-nth`](https://clojuredocs.org/clojure.core/rand)
- [Documentazione di Clojure sulla libreria `alea`](https://clojure.github.io/alea/)
- [Articolo di Medium sulla generazione di numeri casuali in Clojure](https://medium.com/@kumarshubham1987/random-number-generation-in-clojure-5a4a13f4bace)