---
title:                "Conversione di una data in una stringa"
date:                  2024-01-20T17:36:22.651966-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una data in una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Trasformare una data in stringa significa convertire il formato data in testo leggibile. I programmatori lo fanno per mostrare date in formati diversi o per salvare su database non supportanti il tipo data.

## How to:
In Elm, usiamo il modulo `Date` per gestire le date e `elm/time` per la formattazione. Ecco un esempio:

```Elm
import Time exposing (Posix)
import Date exposing (Date)
import Date.Format exposing (format)

-- Supponiamo di avere una data Posix
exampleDate : Posix
exampleDate = ...

convertDateToString : Posix -> String
convertDateToString date =
    date
        |> Date.fromPosix
        |> format "dd/MM/yyyy"

-- Uso della funzione
main =
    convertDateToString exampleDate
    -- Ad esempio, "25/03/2023"
```

## Deep Dive
Elm, essendo funzionale e tipato staticamente, affronta la manipolazione delle date in modo prevedibile. Il pacchetto `elm/time` venne introdotto in Elm 0.19, sostituendo il precedente `elm-lang/core`. Prima di `elm/time`, formattare date era più contorto.

Alternative comuni:
- Usare `Date.toIsoString` per un formato standard ISO-8601.
- Scrivere funzioni personalizzate per formati specifici.

Dettagli implementativi:
- Posix rappresenta istanti temporali con precisione al millisecondo dal 1° gennaio 1970, noto come Unix Epoch.
- `elm/time` non gestisce timezone e DST direttamente – dipende da come li gestisce il browser.

## See Also
- [Elm Date](http://package.elm-lang.org/packages/elm/time/latest/Date)
- [Elm Time](http://package.elm-lang.org/packages/elm/time/latest/Time)
- [Date Format](http://package.elm-lang.org/packages/ryannhg/date-format/latest/) – per formati di data più complessi.
- [ISO-8601 on Wikipedia](https://it.wikipedia.org/wiki/ISO_8601) – per comprendere il formato di data standard.
