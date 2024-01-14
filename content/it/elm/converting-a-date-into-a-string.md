---
title:    "Elm: Convertire una data in una stringa"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

La conversione di una data in una stringa è un'operazione comune nella programmazione di Elm, soprattutto quando si lavora con dati provenienti da fonti esterne come API. Conoscere come fare questa conversione può facilitare il lavoro di manipolazione dei dati e renderlo più preciso.

## Come fare

Per convertire una data in una stringa in Elm, è possibile utilizzare la funzione `DateTime.toIsoString` del modulo `elm/time`. Questa funzione accetta un oggetto `DateTime` e restituisce una stringa nel formato ISO 8601. Ad esempio:

```Elm
import DateTime exposing (..)

myDate = fromYearMonthDay 2021 10 3
DateTime.toIsoString myDate
```

Questa funzione è utile perché il formato ISO 8601 è standardizzato e facilmente interpretabile da altri linguaggi di programmazione o da servizi web.

## Approfondimenti

Ci sono diverse alternative per la conversione di una data in una stringa, come ad esempio l'utilizzo delle funzioni `Date.toGregorian` o `Date.toUnique`. Inoltre, è importante prestare attenzione ai fusi orari quando si manipolano le date, in modo da ottenere i risultati desiderati.

## Vedi anche

- [Modulo DateTime di Elm](https://package.elm-lang.org/packages/elm/time/latest/DateTime)
- [Documentazione su Date.toGregorian](https://package.elm-lang.org/packages/elm/time/latest/Date#toGregorian)
- [Documentazione su Date.toUnique](https://package.elm-lang.org/packages/elm/time/latest/Date#toUnique)
- [ISO 8601 su Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)