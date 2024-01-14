---
title:                "Elm: Convertire una data in una stringa"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molteplici casi in cui è necessario convertire una data in una stringa durante la programmazione in Elm. Ad esempio, quando si lavora con date e orari, è importante essere in grado di visualizzarli in modo leggibile per l'utente. Inoltre, è spesso richiesto di inviare dati formattati a API esterne o di archiviare le date come stringhe nei database.

## Come

Per convertire una data in una stringa in Elm, possiamo utilizzare la funzione `String.fromDate` presente nel modulo `Date`. Questa funzione richiede due parametri: la data e un formato specifico per la stringa di output. Ecco un esempio di come utilizzarla:

```Elm
import Date exposing (..)
import String exposing (..)

myDate = Date.fromYearMonthDay 2020 04 01
dateString = String.fromDate "dd/MM/yyyy" myDate

```

L'output del codice sarà la stringa `"01/04/2020"`. Come potete vedere, abbiamo utilizzato il formato `dd/MM/yyyy` per ottenere il giorno, il mese e l'anno separati da `/`.

Altre opzioni di formattazione disponibili includono `MM/dd/yyyy` per invertire l'ordine del mese e del giorno, `yyyy/MM/dd` per visualizzare la data nel formato ISO, e molti altri. È possibile trovare la lista completa dei formati supportati nella documentazione di Elm sul modulo `Date`.

## Deep Dive

È importante notare che la funzione `String.fromDate` utilizza il sistema di localizzazione predefinito del browser per formattare la data. Ciò significa che il formato della stringa di output può variare a seconda del paese o della lingua del browser dell'utente.

Inoltre, è possibile utilizzare la funzione `Date.newZone` per specificare un fuso orario specifico quando si lavora con date e orari in Elm.

Per maggiori informazioni sulla gestione delle date e degli orari in Elm, è consigliato consultare la documentazione ufficiale, in particolare i moduli `Date` e `Time`.

## Vedi anche

- Documentazione di Elm sul modulo `Date`: https://package.elm-lang.org/packages/elm/time/latest/Time
- Documentazione di Elm sul modulo `String`: https://package.elm-lang.org/packages/elm/core/latest/String