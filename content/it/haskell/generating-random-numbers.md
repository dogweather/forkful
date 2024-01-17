---
title:                "Generazione di numeri casuali"
html_title:           "Haskell: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Generare numeri casuali è il processo di ottenere numeri in maniera casuale, senza un pattern definito. I programmatori lo fanno per diversi motivi, come ad esempio creare giochi con elementi di casualità, testare algoritmi o generare dati simulati per sperimentare con il codice.

## Come Fare:
Per generare numeri casuali in Haskell, è necessario utilizzare il modulo "System.Random". Questo modulo contiene diverse funzioni per generare numeri casuali, incluse "random" e "randomR" per numeri interi e "randomIO" e "randomRIO" per numeri in virgola mobile.

```Haskell
import System.Random

-- Genera un numero casuale intero tra 0 e 10
randomNumber :: IO Int
randomNumber = randomRIO (0, 10)

-- Genera un numero casuale in virgola mobile tra 0 e 1
randomFloat :: IO Float
randomFloat = randomIO
```

## Approfondimento:
La generazione di numeri casuali ha origini antiche ed è stata utilizzata per secoli in molteplici campi, come la divinazione e la statistica. In Haskell, il modulo "System.Random" utilizza un algoritmo chiamato "Mersenne Twister" per generare numeri casuali di alta qualità.

Esistono anche alternative per generare numeri casuali in Haskell, come il modulo "Random" che utilizza l'algoritmo "Simple Good Generator". Inoltre, è stato sviluppato un modulo chiamato "random-fu" che offre funzioni più avanzate per la generazione di numeri casuali.

## Vedi Anche:
- Documentazione ufficiale di Haskell sul modulo "System.Random": https://hackage.haskell.org/package/random/docs/System-Random.html
- Discussione su Reddit sull'utilizzo del modulo "System.Random": https://www.reddit.com/r/haskell/comments/bi7grj/systemrandom_vs_random/
- Documentazione di "random-fu": https://hackage.haskell.org/package/random-fu/docs/System-Random-Fu.html