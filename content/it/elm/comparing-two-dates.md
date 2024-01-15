---
title:                "Confrontare due date"
html_title:           "Elm: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti dover confrontare due date nel tuo codice, ad esempio per verificare la validità di una data di nascita o per implementare una funzionalità di filtro per la data. Imparare a confrontare efficientemente due date in Elm ti aiuterà a migliorare le tue abilità di programmazione e rendere il tuo codice più robusto.

## Come fare

Per confrontare due date in Elm, possiamo utilizzare alcune funzioni predefinite offerte dalla libreria `Date`. Ad esempio, possiamo utilizzare `Date.fromString` per trasformare una stringa in un valore di tipo `Date` e successivamente utilizzare `Date.compare` per confrontare le due date. Vediamo un esempio pratico:

```
import Date exposing (Date, compare, fromString)

-- Trasformiamo due stringhe in date
date1 = fromString "2021-01-15"
date2 = fromString "2021-01-20"

-- Confrontiamo le due date e otteniamo un risultato di tipo Ordering
comparison = compare date1 date2

-- Stampiamo il risultato
case comparison of
    LT -> "date1 è precedente a date2"
    EQ -> "date1 e date2 sono uguali"
    GT -> "date1 è successiva a date2"
```

L'output di questo esempio sarà "date1 è precedente a date2", poiché la data 15 gennaio 2021 è precedente alla data 20 gennaio 2021.

## Approfondimento

La funzione `compare` utilizzata nell'esempio fa parte di una classe di tipi chiamata `Orderable`, che viene implementata automaticamente per qualsiasi tipo di dato ordinabile in Elm. Ciò significa che possiamo utilizzare la stessa logica di confronto per tipi di dato diversi, come ad esempio `Int` o `Float`.

Inoltre, la libreria `Date` offre altre funzioni utili per manipolare e confrontare le date, come ad esempio `Date.add` per aggiungere un determinato numero di giorni o mesi a una data, o `Date.compareDate` per confrontare solo il giorno, il mese e l'anno di due date senza considerare l'ora.

## Vedi anche

Per ulteriori informazioni sulla manipolazione e il confronto delle date in Elm, puoi consultare la documentazione ufficiale della libreria `Date` e il seguente articolo sulla comparazione di date in JavaScript: http://stackabuse.com/Comparing-Dates-in-JavaScript/.