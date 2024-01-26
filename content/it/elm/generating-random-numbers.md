---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:06.538234-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è un modo per ottenere dati imprevedibili. Programmatori lo fanno per giochi, simulazioni o sicurezza.

## How to:
In Elm, si usa `Random` per generare numeri casuali. Ecco un esempio:

```Elm
import Random

-- Genera un numero casuale tra 1 e 10
randomNumber : Random.Generator Int
randomNumber = Random.int 1 10

-- Aggiorna il modello con un numero casuale
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            (model, Random.generate NewRandomNumber randomNumber)

-- Gestisce il nuovo numero casuale
update NewRandomNumber newNumber model =
    ({ model | number = newNumber }, Cmd.none)
```

Esempio di output dopo l'esecuzione:

```Elm
Modello aggiornato con il numero: 7
```

## Deep Dive
Generare numeri casuali non è sempre stato così semplice. In passato, la casualità era ottenuta da eventi fisici esterni. In Elm, la funzione `Random` usa un generatore pseudo-casuale deterministico che necessita di un "seed", ovvero un valore di partenza. I metodi alternativi includono l'utilizzo di API di sistema o servizi esterni per una vera casualità, ma Elm sceglie la prevedibilità e la riproducibilità. l'implementazione dei generatori nel linguaggio facilita il testing e il debugging.

## See Also
- [Random documentation in Elm official guide](https://package.elm-lang.org/packages/elm/random/latest/)
- [Elm Seeds e randomness](https://guide.elm-lang.org/effects/random.html)
