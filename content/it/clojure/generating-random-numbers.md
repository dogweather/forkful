---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cosa e Perché?

La generazione di numeri casuali è il processo di creazione di sequenze numeriche che non presentano alcun modello facilmente riconoscibile. I programmatori lo fanno per una varietà di motivi, tra cui testare il software e simulare situazioni del mondo reale.

## Come fare:

Generare numeri casuali in Clojure è piuttosto semplice. Usando il modulo java.util.Random e la funzione "rand", possiamo generare numeri casuali compresi tra 0 e 1. 

```Clojure
(import java.util.Random)

(let [r (new Random)]
(.nextInt r 100))
```

Questo codice genererà un numero casuale tra 0 e 99. 

## Approfondimento

Historicamente, la generazione di numeri casuali è sempre stata una parte critica della programmazione. Durante la seconda guerra mondiale, ad esempio, i numeri casuali sono stati usati nella crittografia.

Il modulo java.util.Random non è l'unico strumento che Clojure offre per generare numeri casuali. Puoi anche usare funzioni come "rand-int" e "rand-nth".

Dettagli di implementazione: la funzione "rand" restituisce un double. Quindi, se hai bisogno di un numero intero, potresti volerlo arrotondare usando la funzione "round". 

```Clojure
(round (rand 100))
```

Questo codice restituirà un numero intero casuale tra 0 e 100. 

## Guarda Anche

Per un ulteriore approfondimento sulla generazione di numeri casuali in Clojure, visita questi link:

1. Documentazione ufficiale di Clojure - [Random numbers](https://clojuredocs.org/clojure.core/rand)
2. StackOverflow - [Clojure - generate random number](https://stackoverflow.com/questions/2655009/clojure-generate-random-number)