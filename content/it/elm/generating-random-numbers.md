---
title:                "Elm: Generazione di numeri casuali"
programming_language: "Elm"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché Generare Numeri Casuali?

Ci sono molte ragioni per cui si potrebbe voler generare numeri casuali usando il linguaggio di programmazione Elm. Ad esempio, potresti essere un game developer che ha bisogno di creare un sistema di gioco basato sulla casualità, oppure potresti essere un data scientist che ha bisogno di dati casuali per testare un algoritmo. Qualunque sia il motivo, generare numeri casuali è un'abilità utile da avere in Elm.

## Come Generare Numeri Casuali in Elm

In Elm, è possibile generare numeri casuali utilizzando la funzione `Random.generate`. Di seguito è riportato un esempio di codice che genera un numero intero casuale compreso tra 1 e 10:

```Elm
import Random

generateNumber : Cmd Msg
generateNumber =
  Random.generate NewNumber (Random.int 1 10)
```

Questo codice importa il modulo `Random` e utilizza la funzione `generate` per creare un comando che genererà un numero casuale. Il primo argomento passato alla funzione è un messaggio che verrà inviato al tuo model quando il numero è generato. Il secondo argomento è una generatore che definisce il tipo di numero che si desidera generare. In questo caso, stiamo utilizzando la funzione `int` per generare un intero compreso tra 1 e 10.

È anche possibile generare numeri casuali a virgola mobile utilizzando la funzione `float`. Inoltre, è possibile passare un generatore personalizzato alla funzione `generate` per creare numeri casuali in base a criteri specifici.

Puoi utilizzare il comando generato in un `update` della tua app per salvare il numero casuale nel tuo model e utilizzarlo come desideri.

## Approfondimento sulla Generazione di Numeri Casuali

La funzione `Random.generate` utilizza il generatore passato come argomento per creare un `Random.Generator`, che a sua volta viene utilizzato per creare un generatore di numeri casuali. I generatori di numeri casuali sono immutabili e possono essere combinati utilizzando la funzione `map`. Ciò consente di creare numeri casuali complessi, come una lista di numeri casuali o una coppia di numeri casuali.

È importante notare che i numeri casuali generati in Elm sono deterministici. Ciò significa che, dato un determinato generatore, verrà sempre generato lo stesso numero. Tuttavia, è possibile utilizzare una funzione `seed` come argomento per la funzione `Random.generate` per ottenere numeri casuali diversi in base al momento in cui si chiama la funzione.

## Vedi Anche

- Documentazione su Random in Elm: https://package.elm-lang.org/packages/elm/random/latest/
- Esempi pratici di generazione di numeri casuali in Elm: https://dev.to/emilywood/using-random-numbers-in-elm-apps-5h98