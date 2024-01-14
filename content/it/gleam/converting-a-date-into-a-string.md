---
title:                "Gleam: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perchè

Spesso durante la programmazione, ci si confronta con la necessità di convertire una data in una stringa. Ci possono essere vari motivi per cui si vorrebbe fare ciò: ad esempio, per visualizzare la data in un formato specifico o per confrontare due date. In questo articolo vedremo come fare questa operazione utilizzando il linguaggio di programmazione Gleam.

## Come fare

Per convertire una data in una stringa in Gleam, possiamo utilizzare la funzione `Date.iso8601`, che accetta come input una data e restituisce una stringa nel formato ISO 8601, come mostrato nell'esempio seguente:

```Gleam
let data = Date.from_calendar(2021, 1, 1, 0, 0, 0, "UTC")
let data_string = Date.iso8601(data)

// Output
"2020-12-31T00:00:00+00:00"
```

Possiamo anche specificare un fuso orario diverso da quello predefinito, utilizzando il parametro opzionale `time_zone`, come nell'esempio seguente:

```Gleam
let data = Date.from_calendar(2021, 1, 1, 0, 0, 0, "UTC")
let data_string = Date.iso8601(data, time_zone = "Europe/Rome")

// Output
"2021-01-01T01:00:00+01:00"
```

Inoltre, è possibile personalizzare il formato della stringa utilizzando la funzione `Date.format`, che accetta come input una data, una stringa di formato e, opzionalmente, un fuso orario, come mostrato nell'esempio seguente:

```Gleam
let data = Date.from_calendar(2021, 1, 1, 12, 30, 0, "UTC")
let data_string = Date.format(data, "%Y/%m/%d - %H:%M:%S", time_zone = "Europe/Rome")

// Output
"2021/01/01 - 13:30:00"
```

## Approfondimento

La conversione di una data in una stringa può sembrare un'operazione semplice, ma è importante prestare attenzione ai dettagli, come il fuso orario e il formato desiderato. Inoltre, in Gleam è anche possibile manipolare le date utilizzando la libreria `DateTime`, che offre una serie di funzioni utili per calcolare differenze tra date, aggiungere o sottrarre giorni, ore, minuti o secondi a una data, e molto altro.

## Vedi anche

- [Documentazione di Gleam su Date e DateTime](https://gleam.run/documentation/standard-library/date/)
- [Tutorial di Gleam su le date e l'orario](https://gleam.run/articles/dates-and-times/)
- [ISO 8601](https://it.wikipedia.org/wiki/ISO_8601)