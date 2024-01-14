---
title:                "Elm: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività molto utile nella programmazione. Ad esempio, può essere utilizzato per creare giochi con risultati casuali o per generare dati casuali per scopi di testing.

## Come fare

Per generare numeri casuali in Elm, è possibile utilizzare la funzione `Random.int` che prende come argomenti un numero minimo e un numero massimo e genera un intero casuale compreso tra questi due valori.

```Elm
import Random

Random.int 1 10
-- questo genererà un numero casuale compreso tra 1 e 10 (inclusi)
-- output: 7
```

È anche possibile generare numeri casuali float utilizzando la funzione `Random.float` e specificando un intervallo.

```Elm
import Random

Random.float 0 1
-- questo genererà un numero casuale float compreso tra 0 e 1 (inclusi)
-- output: 0.6482234
```

È importante notare che quando si utilizzano numeri float, si consiglia di specificare una precisione con la funzione `Random.floatRange` per evitare problemi di approssimazione.

## Approfondiamo

La generazione di numeri casuali in Elm è basata sull'utilizzo del generatore casuale, che prende come parametro un seed e restituisce un nuovo seed e un valore casuale. Il seed può essere visto come il punto di partenza per la generazione dei numeri casuali e viene utilizzato per garantire che i numeri generati siano sempre gli stessi quando si passa lo stesso seed.

```Elm
import Random

generator = Random.initialSeed 42
-- questo restituisce un generatore casuale con il seed 42

newSeedAndValue = Random.generate Random.int 1 10 generator
-- questo utilizza il generatore per generare un nuovo seed e un valore che corrisponde a un intero casuale compreso tra 1 e 10 (inclusi)
```

Il generatore casuale può anche essere combinato utilizzando le funzioni `andThen`, `map` e `map2` per generare valori più complessi o per utilizzare numeri casuali in contesti diversi.

## Vedi anche

- Documentazione ufficiale di Elm sulla generazione di numeri casuali: https://package.elm-lang.org/packages/elm/random/latest/
- Un tutorial su come utilizzare i numeri casuali in Elm: https://dev.to/filipowm/generating-random-numbers-in-elm-3b1d
- Un esempio di creazione di un gioco utilizzando i numeri casuali in Elm: https://medium.com/@_sulrich/creating-a-game-with-elm-796c66bd7148