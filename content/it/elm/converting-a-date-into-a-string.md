---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
La conversione di una data in una stringa significa trasformare un oggetto data in testo leggibile. I programmatori lo fanno per semplificare la rappresentazione e la visualizzazione delle date.

## Come fare:
Utilizza la funzione `toString` di Elm per convertire una data in una stringa.

```Elm
import Time

main =
    let
        time = Time.millisToPosix 1463480130000
    in
    text (Time.toIsoString time)
```

Questo produrrà un output come:

```Elm
"2016-05-17T08:02:10Z"
```

In questo esempio, abbiamo prima convertito i millisecondi in un oggetto POSIX e poi l'abbiamo convertito in una stringa ISO.

## Approfondimento
La conversione delle date in stringhe esiste da quando le date sono state introdotte nei linguaggi di programmazione. Elm usa lo standard internazionale ISO 8601 per la conversione, assicurando che le stringhe risultanti siano universali.

Ci sono alternative, come l'uso di librerie esterne o la creazione di una funzione personalizzata per adattare l'output alle tue esigenze. Tuttavia, la funzione `toIsoString` incorporata di Elm è generalmente l'approccio più semplice e consigliato.

L'implementazione di `toIsoString` in Elm segue l'ora UTC. Questo significa che la stringa prodotta è sempre in relazione all'orario GMT (Greenwich Mean Time), non all'ora locale.

## Vedi Anche
- Documentazione Elm su [Time.toIsoString](https://package.elm-lang.org/packages/elm/time/latest/Time#toIsoString)
- Standard [ISO 8601](https://it.wikipedia.org/wiki/ISO_8601) su Wikipedia
- ISO 8601 in Elm: [Date and Time Formats](https://discourse.elm-lang.org/t/iso-8601-date-and-time-formats/2997)
- Esempi di codice Elm per manipolare le date: [Elm Date Examples](https://ellie-app.com/a/Dm63Q8b9N7ja1)