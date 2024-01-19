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

## Cos'è & Perché?

Calcolare una data futura o passata significa determinare una data basata su un certo numero di giorni o mesi prima o dopo una data specifica. I programmatori spesso lo fanno per gestire scadenze, programmazioni e per monitorare intervalli di tempo.

## Come fare:

Ecco un esempio di codice in Elm per calcolare una data futura o passata. Ci basiamo sulla libreria `Date.Extra` per questa operazione.

```Elm
import Date
import Date.Extra as Date


dataInFuturo : Date.Date
dataInFuturo =
    Date.add
        { days = 30, months = 1, years = 0 }
        (Date.fromCalendarDate 2023 1 1)


dataInPassato : Date.Date
dataInPassato =
    Date.add
        { days = -15, months = -6, years = -2 }
        (Date.fromCalendarDate 2023 1 1)
```

Questo esempio calcola una data 30 giorni e 1 mese nel futuro e una data 15 giorni, 6 mesi e 2 anni nel passato, entrambe a partire dal 1 gennaio 2023.

## Approfondimenti:

Elm è un linguaggio funzionale che enfatizza la sicurezza dei tipi e l'immunità agli errori runtime, rendendolo ideale per il calcolo di date futuri o passati. I concetti di Elm per il calcolo delle date deriva direttamente da Haskell, un altro linguaggio di programmazione funzionale di alto livello.

Come alternativa al calcolo diretto, è possibile utilizzare funzioni di terze parti come `Date.dayOfYear` o `Date.dayOfEpoch` per ottenere informazioni su una data specifica e quindi aggiungere o sottrarre giorni di conseguenza.

Ricordati, tuttavia, dell’importanza della gestione di date speciali, come il 29 febbraio nei anni bisestili o il fuso orario. Di conseguenza, potrebbe essere necessario tenere conto di questi dettagli durante l'implementazione.

## Vedi anche:

Per ulteriori informazioni e guidare con Elm e il calcolo di date, consulta le seguenti risorse:

- Documentazione ufficiale Elm per Date: https://package.elm-lang.org/packages/elm/time/latest/
- Libreria 'Date.Extra': https://package.elm-lang.org/packages/elm-community/date-extra/latest/
- Guida alla programmazione con Elm: https://guide.elm-lang.org/