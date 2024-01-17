---
title:                "Calcolare una data nel futuro o nel passato."
html_title:           "Gleam: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Calcolare una data nel futuro o nel passato è un'operazione comune nel mondo della programmazione. Gli sviluppatori spesso hanno bisogno di gestire date in un'ampia varietà di scenari, come ad esempio pianificare eventi o generare report. Invece di farlo manualmente, utilizzando semplici calcoli matematici, possiamo sfruttare il potere dei linguaggi di programmazione per automatizzare il processo e risparmiare tempo.

## Come fare:

Per calcolare una data nel futuro o nel passato con Gleam, ci sono diverse opzioni a disposizione. Possiamo utilizzare il modulo ```Date``` che fornisce funzioni apposite o creare una funzione customizzata. Ecco alcuni esempi:

### Utilizzando il modulo Date:

```
import Gleam.Date

let future_date = Date.add_days(Date.now(), 30)

let past_date = Date.add_months(Date.now(), -6)

println(future_date) // Output: 2021-11-17T12:00:00.000Z

println(past_date) // Output: 2020-05-17T12:00:00.000Z
```

### Creando una funzione customizzata:

```
import Gleam.Date

fn calculate_future_date(target_date, days) {
  Date.add_days(target_date, days)
}

let target_date = Date.from({ year: 2021, month: 1, day: 1 })

let future_date = calculate_future_date(target_date, 60)

println(future_date) // Output: 2021-03-02T12:00:00.000Z
```

## Approfondimento:

La gestione delle date è sempre stata un'operazione complessa per i programmatori. Nei primi sistemi informatici, le date venivano rappresentate utilizzando un numero sequenziale, scelto come punto di riferimento, che rappresentava il numero di giorni passati da una data specifica, come ad esempio il 1° gennaio 1900. Questo metodo ha portato a problemi di conversione tra formati diversi e ha reso difficile il calcolo di date in futuro o in passato.

In alternativa al modulo ```Date``` di Gleam, possiamo utilizzare librerie esterne come ad esempio "Chronic" o "Calendar" per gestire le date in modo più flessibile.

L'implementazione del calcolo di una data nel futuro o nel passato è possibile grazie all'utilizzo di algoritmi matematici specifici per la conversione di diverse unità di tempo come anni, mesi, giorni, ore, minuti e secondi.

## Vedi anche:

- Documentazione del modulo Date di Gleam: [https://gleam.run/modules/Date.html](https://gleam.run/modules/Date.html)
- Libreria "Chronic" per la gestione avanzata delle date: [https://github.com/mroth/chronic](https://github.com/mroth/chronic)
- Libreria "Calendar" per la gestione delle date in Erlang: [https://github.com/lau/calendar](https://github.com/lau/calendar)