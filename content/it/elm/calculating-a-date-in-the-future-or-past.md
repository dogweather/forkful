---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Elm: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Il calcolo di una data in futuro o passato è semplicemente il processo di determinare una data specifica in base a una data di riferimento e un numero di unità temporali da aggiungere o sottrarre. I programmatori spesso eseguono questo tipo di calcolo quando devono gestire le date in un'applicazione o un sito web.

## Come si fa:

Elm offre una varietà di funzioni utili per calcolare date in futuro o passato. Ecco alcuni esempi:

```Elm
import Time exposing (..)

-- Calcola una data futura aggiungendo un certo numero di giorni alla data di riferimento
addDays 5 [30, 1, 2022] -- output: [4, 2, 2022]

-- Calcola una data passata sottraendo un certo numero di mesi dalla data di riferimento
subMonths 3 [15, 6, 2021] -- output: [15, 3, 2021]

-- Calcola una data futura aggiungendo un certo numero di secondi alla data di riferimento
addSeconds 3600 [18, 9, 2021] -- output: [19, 9, 2021]
```

## Approfondimenti:

Il calcolo delle date è una parte essenziale della programmazione poiché permette di gestire in modo efficiente le informazioni temporali. Oltre alle funzioni fornite da Elm, ci sono anche altre alternative come l'utilizzo di librerie esterne o la scrittura di funzioni personalizzate per il proprio progetto. Per calcoli più complessi, è consigliabile leggere la documentazione di Elm per saperne di più sulle funzioni disponibili.

## Vedi anche:

Per ulteriori informazioni sulle funzioni di calcolo delle date di Elm, puoi consultare la documentazione ufficiale su [Time](https://package.elm-lang.org/packages/elm/time/latest/Time) e [Date](https://package.elm-lang.org/packages/elm/time/latest/Date).