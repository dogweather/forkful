---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Parsare una data da una stringa significa estrarre una data (giorno, mese, anno) da un testo. I programmatori lo fanno per manipolare o utilizzare queste informazioni all'interno del loro codice.

## Come Fai:

Ecco un esempio di come parsare una data da una stringa in Elm:

```Elm
import Date exposing (..)
import Time exposing (..)

stringaDiData = "2021-12-31"

maybeData = Date.fromString stringaDiData

case maybeData of
    Nothing ->
        -- Gestisci l'errore qui
    Just data ->
        -- Usa l'oggetto data qui
```

In questo script, `Date.fromString` tenta di parsare la stringa fornita come un oggetto di data. Nota che `Date.fromString` restituirà `Nothing` se non riuscirà a parsare la stringa, dandoti la possibilità di gestire l'errore.

## Approfondimento

Elm ha introdotto `Date.fromString` in versioni più recenti per aiutare i programmatori a manipolare le date. Un'alternativa comune sarebbe scrivere una funzione personalizzata di parsing, ma `Date.fromString` è generalmente più affidabile e semplice da usare.

Implementare la funzione `Date.fromString` richiede la comprensione delle particolari convenzioni di formattazione di data e ora come ISO 8601. Tieni in considerazione che `Date.fromString` attualmente supporta solo un sottoinsieme di ISO 8601.

## Vedi Anche

Per ulteriori informazioni consulta la documentazione ufficiale Elm qui: http://package.elm-lang.org/packages/elm/time/latest/

Per chi fosse interessato ad approfondire le specifiche del formato di data e ora ISO 8601, consigliamo l'articolo di Wikipedia: https://it.wikipedia.org/wiki/ISO_8601