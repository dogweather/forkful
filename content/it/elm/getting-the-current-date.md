---
title:                "Elm: Ottenere la data corrente"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché Utilizzare Elm per Ottenere la Data Corrente?

Ottenere la data corrente potrebbe sembrare un'operazione banale, ma in realtà può essere molto utile in diversi scenari di programmazione. Con Elm, puoi facilmente ottenere la data corrente e utilizzarla nel tuo codice in modo efficiente e accurato.

## Come Ottenere la Data Corrente in Elm

Per ottenere la data corrente in Elm, possiamo utilizzare la funzione `Date.now` del pacchetto `elm/time`. Questa funzione restituisce un valore `Time` rappresentante la data corrente, che possiamo poi formattare a nostro piacimento.

```Elm
import Time exposing (..)
import Date exposing (..)

currentTime : Time
currentTime =
    Date.now

-- Output: 1604585419200
```

Possiamo anche utilizzare il package `elm/time` per formattare la data corrente in un formato più leggibile, come ad esempio una stringa.

```Elm
import Time exposing (..)
import Date exposing (..)

currentTime : String
currentTime =
    Time.format "%Y/%m/%d" (Date.now)

-- Output: 2020/11/05
```

## Approfondimento su Come Funziona l'Ottenimento della Data Corrente in Elm

La funzione `Date.now` sfrutta il time Unix epoch, un sistema utilizzato per rappresentare il tempo come numero di secondi passati dal 1 gennaio 1970. In Elm, il package `elm/time` converte questo valore in un tipo `Time` per rappresentare la data corrente.

Per formattare la data, il package `elm/time` utilizza la libreria di formattazione time di JavaScript, fornendo una serie di stringhe di formattazione per personalizzare il risultato finale.

## Vedi Anche

- Documentazione ufficiale del package `elm/time` su https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial su come utilizzare Elm con Time di Luca Palmieri su https://medium.com/@lucapalmieri/introduzione-ad-elm-con-time-b7e645e6c68c