---
title:                "Haskell: Generazione di numeri casuali"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Il generare numeri casuali è utile in diversi ambiti della programmazione, dalle simulazioni ai giochi, fino allo sviluppo di algoritmi di machine learning. Con Haskell, possiamo generare numeri casuali in modo sicuro e controllato, e questo rende la scelta di utilizzare questo linguaggio ancora più attraente.

## Come fare
Per generare numeri casuali in Haskell, utilizziamo la funzione `randomR` del modulo `System.Random`. Possiamo specificare un intervallo di numeri in cui vogliamo che il numero casuale sia generato, ad esempio:

```Haskell
randomR (1, 10) :: IO Int
```

Questa funzione restituisce un valore di tipo `IO Int`, il che significa che viene eseguita all'interno del mondo della IO e, quindi, produce un numero casuale ogni volta che viene chiamata. Possiamo anche generare un numero casuale di tipo `Double`, specificando il range corrispondente:

```Haskell
randomR (0.0, 1.0) :: IO Double
```

Se invece vogliamo ottenere un numero casuale in un range specifico senza essere limitati a un tipo specifico, possiamo utilizzare la funzione `random`:

```Haskell
random :: (Random a, RandomGen g) => g -> (a, g)
```

Questa funzione restituisce un numero casuale e un nuovo generatore, utile se ci troviamo a dover generare più numeri casuali all'interno della stessa esecuzione.

## Approfondimento
Per generare numeri casuali in modo sicuro e affidabile, Haskell utilizza un concetto chiamato "generatore". Il generatore è in realtà un'implementazione dell'algoritmo di generazione di numeri pseudo-casuali, che utilizza un seed (seme) per generare i numeri. Questo significa che se utilizziamo lo stesso seed, otterremo sempre gli stessi numeri casuali. Possiamo anche utilizzare una "carta di gioco" come seed, ottenendo così una sequenza di numeri casuali che riproduce lo stesso ordine delle carte in un mazzo.

Inoltre, possiamo controllare il seed utilizzato dal generatore, in modo da poter riprodurre sempre la stessa sequenza di numeri casuali. Questo è particolarmente utile durante la fase di debug, quando dobbiamo analizzare un comportamento dipendente dai numeri casuali.

## Vedi anche
- [Documentazione ufficiale di Haskell sulla generazione di numeri casuali](https://www.haskell.org/onlinereport/random.html)
- [Tutorial su come generare numeri casuali in Haskell](https://www.codewars.com/kata/54c3a5939aeeceeae1001000)
- [Esempi pratici di utilizzo della generazione di numeri casuali in Haskell](https://williamyaoh.com/posts/2015-09-11-Haskell-Random/index.html)