---
title:                "Convertire una data in una stringa"
html_title:           "Elm: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Convertire una data in una stringa è un'operazione comune per i programmatori. Ci permette di visualizzare una data in un formato più leggibile per gli utenti finali. Ad esempio, invece di mostrare una data come "2021-05-24", potremmo volerla visualizzare come "24 maggio 2021". Questo rende le informazioni più comprensibili e user-friendly.

## Come fare:
Ecco un esempio di codice Elm che converte una data in una stringa nel formato "gg MMMM aaaa", utilizzando la libreria integrata `Date` di Elm:
```elm
import Date exposing (Day, Month, Year, toIsoString)

dateToString : Day -> Month -> Year -> String
dateToString day month year =
    toIsoString (Date.fromMonthYear day month year)
    |> String.split "-" 
    |> List.reverse 
    |> String.join " "
    |> String.toUpper
```
Output:
```elm
dateToString 24 May 2021
"24 MAGGIO 2021"
```

## Approfondimento:
In passato, le date venivano rappresentate in modo diverso nei vari paesi e culture. Ciò ha portato a diverse convenzioni e formati per mostrare le date. Con l'avvento del computer e della standardizzazione dei formati di dati, come l'ISO 8601, è diventato più semplice convertire le date in una stringa.

Esistono anche altre librerie di terze parti in Elm, come `alwaysAI/elm-date-format` e `dillonkearns/elm-local-datetime`, che offrono funzioni più avanzate per la gestione delle date.

Quando si lavora con date in Elm, è importante prestare attenzione al fuso orario e alla differenza tra tempo locale e UTC. La libreria `Date` di Elm utilizza il fuso orario per impostazione predefinita e può causare problemi se non viene gestita correttamente.

## Vedi anche:
- Documentazione ufficiale della libreria `Date` di Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Articolo sul formato delle date ISO 8601: https://it.wikipedia.org/wiki/ISO_8601
- Esempi di altri formati per le date: https://en.wikipedia.org/wiki/Date_format_by_country