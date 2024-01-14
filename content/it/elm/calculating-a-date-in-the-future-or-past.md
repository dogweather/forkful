---
title:                "Elm: Calcolare una data nel futuro o nel passato"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni, come ad esempio la programmazione di eventi o la gestione di scadenze.

## Come fare

Per calcolare una data in Elm, esistono due opzioni: utilizzare la libreria "elm/time" o scrivere una funzione personalizzata. Se si sceglie di utilizzare la libreria, ecco un semplice esempio:

```
import Time exposing (..)

addDays 7 (fromUtcTime 1593000000)

-- Output: 2020-06-29T00:00:00+00:00
```

In questo esempio, stiamo aggiungendo 7 giorni alla data 1593000000, che corrisponde al primo luglio 2020. È possibile utilizzare le funzioni della libreria "elm/time" per aggiungere o sottrarre giorni, ore, minuti o secondi da una data specifica.

Se si preferisce scrivere una funzione personalizzata, si può utilizzare la libreria "elm/time-extras" che offre funzioni più avanzate per il calcolo delle date. Ad esempio, la funzione `addDays` prende in input una data e il numero di giorni da aggiungere e restituisce la data modificata.

## Approfondimento

Per calcolare una data in futuro o nel passato, è importante considerare alcune cose. In primo luogo, bisogna essere consapevoli dei fusi orari e utilizzare le funzioni adeguate per convertire le date in formato UTC. Inoltre, bisogna considerare l'eventualità di avere un anno bisestile e gestire correttamente il cambio di anno durante il calcolo.

Un altro aspetto importante è la gestione degli orari estivi e invernali, che possono influire sulla durata di un giorno. In questi casi, è necessario utilizzare funzioni più avanzate che tengano conto di queste variazioni per avere risultati precisi.

## Vedi anche

- [Documentazione della libreria "elm/time"](https://package.elm-lang.org/packages/elm/time/latest/)

- [Documentazione della libreria "elm/time-extras"](https://package.elm-lang.org/packages/elm-explorations/time/latest/)

- [Articolo sull'utilizzo dei fusi orari in Elm](https://thoughtbot.com/blog/working-with-time-and-time-zones-in-elm)

- [Esempi di calcolo delle date in Elm](https://elmprogramming.com/elm-dates.html)