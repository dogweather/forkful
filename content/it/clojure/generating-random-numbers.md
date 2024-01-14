---
title:    "Clojure: Generazione di numeri casuali"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è utile per una varietà di scopi, come ad esempio nella creazione di giochi o per testare algoritmi. Inoltre, può essere interessante dal punto di vista matematico ed esplorare i diversi modi in cui è possibile generare numeri casuali.

## Come procedere

Per generare numeri casuali in Clojure, è possibile utilizzare la funzione `rand`. Questa funzione prende una quantità opzionale di argomenti, specificando il range dei numeri da generare. Ad esempio, per generare un numero casuale tra 1 e 10, possiamo scrivere il seguente codice:

```Clojure
(rand 1 10)
```

Questo produrrà un numero intero casuale tra 1 e 10. È anche possibile utilizzare numeri decimali come argomenti per `rand`.

Per generare più di un numero casuale, possiamo utilizzare la funzione `take`, che prende due argomenti: il numero di elementi da prendere e la sequenza da cui prendere gli elementi. Ad esempio, per generare una sequenza di 5 numeri casuali tra 1 e 10, possiamo scrivere il seguente codice:

```Clojure
(take 5 (repeatedly #(rand 1 10)))
```

Questo produrrà una lista di 5 numeri casuali.

## Approfondimento

Nella generazione di numeri casuali è importante considerare la distribuzione dei numeri generati. La funzione `rand` utilizza una distribuzione uniforme, il che significa che ogni numero ha la stessa probabilità di essere generato. Se si desidera una distribuzione diversa, è possibile utilizzare la funzione `rand-nth`, che utilizza una distribuzione gaussiana.

Inoltre, è possibile impostare un seme per la funzione `rand` utilizzando la funzione `set!`, che garantisce la riproducibilità dei numeri casuali generati.

## Vedi anche

- Documentazione su `rand` e `rand-nth`: https://clojuredocs.org/clojure.core/rand
- Esempi di utilizzo di numeri casuali in Clojure: https://www.lihaoyi.com/post/HowClojureScriptCompiles.html#random-numbers
- Approfondimenti sulla generazione di numeri casuali: https://en.wikipedia.org/wiki/Random_number_generation