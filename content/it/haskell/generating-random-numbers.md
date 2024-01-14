---
title:    "Haskell: Generazione di numeri casuali"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è spesso utile nei programmi perché può imitare il comportamento della vita reale. Ad esempio, un gioco di ruolo potrebbe utilizzare numeri casuali per determinare il risultato di un'azione dei giocatori.

## Come fare

Per generare numeri casuali in Haskell, è possibile utilizzare la funzione `randomRIO` del modulo `System.Random`. La sintassi è la seguente:

``` Haskell
randomRIO :: (Random a, RandomGen g) => (a, a) -> g -> (a, g)
```

In questo modo, è possibile specificare un intervallo di numeri e Haskell restituirà un numero casuale all'interno di quell'intervallo. Ad esempio, se volessimo generare un numero casuale compreso tra 1 e 10, potremmo scrivere:

``` Haskell
randomRIO (1, 10) :: IO Int
```

Il tipo di dato `IO Int` indica che la funzione restituisce un valore di tipo `Int` all'interno del contesto di `IO`. Ciò significa che la funzione deve essere eseguita all'interno di un'azione di `IO` per ottenere il risultato.

## Approfondimento

La funzione `randomRIO` utilizza il tipo di dati `RandomGen` per generare numeri casuali. Questo tipo rappresenta un generatore di numeri casuali ed è un'istanza di `Random`. Ciò significa che Haskell ha un'implementazione predefinita di come generare numeri pseudo-casuali.

Per generare numeri casuali effettivi, è possibile utilizzare il modulo `System.Random.TF` che implementa il generatore di numeri casuali di tipo Mersenne Twister. Ciò consente di generare sequenze di numeri casuali più complesse e affidabili.

## Vedi anche

- [Documentazione di randomRIO](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html#v:randomRIO)
- [Documentazione di System.Random.TF](http://hackage.haskell.org/package/random-1.2.0/docs/System-Random-TF.html)
- [Esempi pratici di utilizzo della generazione di numeri casuali in Haskell](https://www.codementor.io/@arpitbhayani/generating-random-number-in-haskell-and-its-uses-oq97cs5ff)