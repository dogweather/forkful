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

## Perché
Se stai lavorando con date in Elm, a un certo punto potresti dover convertire una data in una stringa per mostrarla all'utente o per elaborarla in qualche modo. In questo articolo ti mostrerò come fare questa conversione in modo semplice ed efficiente.

## Come Fare
Per prima cosa, dovrai importare il modulo "Date" all'inizio del tuo codice Elm:
``` Elm
import Date exposing (Date)
```
Una volta importato il modulo, puoi utilizzare la funzione `toIsoString` per convertire una data in una stringa secondo il formato ISO utilizzato in Elm. Ad esempio:
``` Elm
dateToString : Date -> String
dateToString date =
    Date.toIsoString date

date = Date.fromCalendarDate 2020 1 1
dateToString date --> "2020-01-01"
```
Come puoi vedere dall'esempio, la funzione `dateToString` accetta un parametro di tipo `Date` e restituisce una stringa corrispondente.
È anche possibile utilizzare la funzione `format` per specificare un formato personalizzato per la stringa di output. Questa funzione accetta due parametri: una stringa con il formato desiderato e la data da convertire. Per esempio:
``` Elm
dateToString : Date -> String
dateToString date =
    Date.format "DD MMMM YYYY" date

date = Date.fromCalendarDate 2020 1 1
dateToString date --> "01 January 2020"
```
Ci sono molti altri formati che puoi utilizzare, quindi assicurati di consultare la documentazione ufficiale per ulteriori informazioni.

## Approfondimento
Nel modulo "Date" ci sono anche molte altre funzioni utili per lavorare con date in Elm, come `fromPosix` per convertire un timestamp Unix in una data e `fromString` per convertire una stringa in una data. Inoltre, puoi anche utilizzare il modulo "Time" per effettuare operazioni matematiche su date, ad esempio per aggiungere o sottrarre un certo numero di giorni o secondi a una data.

## Vedi Anche
- [Documentazione ufficiale di Elm su Date](https://package.elm-lang.org/packages/elm/time/latest/Time#Posix)
- [Tutorial su come utilizzare date in Elm](https://thoughtbot.com/blog/working-with-dates-in-elm)