---
title:                "Elm: Confrontare due date"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
La comparazione di due date è una funzionalità essenziale per gestire e manipolare date in modo efficace e preciso. Grazie all'utilizzo di Elm, è possibile eseguire facilmente questa operazione senza dover gestire manualmente le complessità delle date.

## Come fare
Per iniziare, dobbiamo prima importare il modulo `Date` di Elm nella nostra applicazione. Questo modulo ci permette di creare oggetti `Date` e utilizzare le sue funzioni per manipolarle e confrontarle.

```
Elm import Date exposing (Date)
```

Dopo l'importazione, possiamo creare due oggetti `Date` da confrontare. Ad esempio, per confrontare la data odierna con quella di ieri, possiamo utilizzare i seguenti codici:

```
Elm today = Date.fromIsoString "2021-05-10"
Elm yesterday = Date.fromIsoString "2021-05-09"

Elm Date.compare yesterday today
```

Questo codice ci restituirà il valore `-1`, indicando che la data di ieri è precedente a quella di oggi. Possiamo sfruttare questo valore per eseguire determinate operazioni in base al risultato del confronto.

## Approfondimento
Il modulo `Date` di Elm ci offre molte altre funzionalità utili per lavorare con le date. Possiamo utilizzare la funzione `Date.fromPosix` per creare un oggetto `Date` da un timestamp Unix, oppure la funzione `Date.fromTime` per creare una data a partire da un oggetto `Time` di Elm.

Inoltre, possiamo utilizzare la funzione `Date.toIsoString` per convertire la nostra data in formato ISO, o la funzione `Date.toTime` per ottenere l'oggetto `Time` corrispondente alla data.

Con l'utilizzo delle funzioni e delle operazioni di confronto del modulo `Date`, possiamo gestire e manipolare le date in modo semplice e preciso nella nostra applicazione Elm.

## Vedi anche
- Documentazione ufficiale del modulo `Date` di Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Articolo di approfondimento sulle operazioni di confronto delle date in Elm: https://medium.com/elm/how-to-compare-dates-in-elm-90a6ef5caec6 
- Esempi di codice per la manipolazione delle date in Elm: https://dev.to/kumssi/date-manipulation-in-elm-with-example-3dja