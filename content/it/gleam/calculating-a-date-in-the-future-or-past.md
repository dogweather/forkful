---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Gleam: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Calcolare una data nel futuro o nel passato significa determinare una data specifica partendo dalla data attuale e muovendosi avanti o indietro nel tempo. I programmatori lo fanno per gestire o pianificare eventi, promemoria o intervalli temporali specifici.

## Come fare:
Ecco un esempio di come calcolare una data nel futuro in Gleam:
```Gleam
import gleam/datetime.{interval, add}

let future_date = datetime
 |> add(interval(days: 7)) // Calcola data di una settimana nel futuro
```
Questo calcola la data di una settimana nel futuro dalla data corrente.

## Approfondimento
Historicamente, il calcolo delle date avveniva manualmente, ma man mano che la programmazione è avanzata, sono state sviluppate librerie di funzioni per gestire queste operazioni.
Un'altra opzione sarebbe l'utilizzo di librerie esterne o costruzioni di linguaggio specifiche, come `Time` in Ruby o le funzioni di data/ora in Javascript.
Per quanto riguarda l'implementazione in Gleam, la funzione `add` della libreria `datetime` prende come argomento un `interval` da aggiungere alla data corrente.

## Vedi anche
- [Understanding Time in Programming](https://medium.com/swlh/understanding-time-in-programming-b5429ef87cbc) per una panoramica generale sulla gestione del tempo nella programmazione.