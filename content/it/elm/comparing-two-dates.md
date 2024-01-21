---
title:                "Confronto tra due date"
date:                  2024-01-20T17:32:54.394997-07:00
model:                 gpt-4-1106-preview
simple_title:         "Confronto tra due date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Comparare due date significa verificare se sono uguali, quale precede l'altra o calcolare la differenza tra di loro. I programmatori lo fanno per organizzare eventi, filtrare dati o per funzionalità legate al tempo come reminder o scadenze.

## How to: (Come fare:)
```Elm
import Time
import Date

-- Definiamo due date
date1 : Date.Posix
date1 =
    Time.millisToPosix 1598918400000  -- corrisponde al 1 settembre 2020

date2 : Date.Posix
date2 =
    Time.millisToPosix 1601510400000  -- corrisponde al 1 ottobre 2020

-- Controllare se le date sono uguali
areDatesEqual : Bool
areDatesEqual =
    date1 == date2

-- Verifica quale data viene prima
whichIsEarlier : Date.Posix
whichIsEarlier =
    if date1 < date2 then date1 else date2

-- Calcolare la differenza in giorni
differenceInDays : Float
differenceInDays =
    Time.posixToMillis date2 - Time.posixToMillis date1
        |> (/) (24 * 60 * 60 * 1000)

-- Utilizzo
areDatesEqual --> False
whichIsEarlier --> Tue Sep 01 2020 00:00:00 GMT+0000
differenceInDays --> 30.0
```

## Deep Dive (Approfondimento)
Comparare date in Elm è semplice, grazie al modulo `Time`. Storicamente, operazioni sul tempo in programmazione hanno creato problemi: fusi orari, cambi di ora legale, bug dell'anno 2000. Elm gestisce il tempo usando il tipo `Posix`, che rappresenta un punto nel tempo in millisecondi dall’epoca Unix (00:00:00 UTC il 1 gennaio 1970). `Posix` evita gran parte della complessità legata alla gestione del tempo. 

Alternative all'uso diretto del modulo `Time` includono librerie di terze parti che forniscono funzionalità extra, come parsing, formattazione o gestione delle zone orarie. Valuta se ne hai bisogno prima di aggiungere dipendenze al tuo progetto.

Dettagli di implementazione: attenzione alle rappresentazioni di date e ore che possono variare a seconda della zona oraria in cui operi; `Posix` ti aiuta, ma è sempre bene verificare il comportamento nelle diverse condizioni.

## See Also (Vedi Anche)
- [Elm Time documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date Format](https://package.elm-lang.org/packages/ryannhg/date-format/latest/) per formattare date in modo leggibile.