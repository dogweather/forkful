---
title:    "Gleam: Calcolare una data nel futuro o nel passato"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui potresti voler calcolare una data nel futuro o nel passato utilizzando il linguaggio di programmazione Gleam. Potresti avere bisogno di calcolare una data di scadenza per un progetto o pianificare un evento in anticipo. Inoltre, la capacità di calcolare una data in modo preciso e affidabile è un'importante abilità richiesta in molti progetti di sviluppo software.

## Come fare

Per calcolare una data nel futuro o nel passato utilizzando Gleam, puoi utilizzare la libreria standard `gleam/time`. Iniziando con una data di riferimento, puoi utilizzare le funzioni `add` e `subtract` per aggiungere o sottrarre un determinato numero di giorni, settimane, mesi o anni.

```Gleam
import gleam/time.{Date, Iso}

// Calcola una data 7 giorni nel futuro a partire da oggi
let today = Date.from_iso(Iso.now())
let future_date = Date.add(today, days: 7)
```

L'output di questo codice sarà la data dei prossimi 7 giorni, rappresentata nel formato `#Date {year: 2021, month: 7, day: 22}`.

Puoi anche utilizzare le funzioni `earliest` e `latest` per calcolare la data più presto o più tardi tra due date fornite.

```Gleam
import gleam/time.{Date, Iso}

// Calcola la data più presto tra due date fornite
let date1 = Date.from_iso(Iso.from_calendar_date(year: 2021, month: 7, day: 20))
let date2 = Date.from_iso(Iso.from_calendar_date(year: 2021, month: 7, day: 25))
let earliest_date = Date.earliest(date1, date2)
```

L'output di questo codice sarà la data `July 20, 2021` poiché è la più presto tra le due date fornite.

## Approfondimento

Calcolare una data nel futuro o nel passato potrebbe sembrare un'operazione semplice, ma può in realtà essere complessa a causa di vari fattori come gli anni bisestili e i fusi orari. Fortunatamente, la libreria `gleam/time` gestisce tutti questi aspetti in modo efficiente, consentendoti di concentrarti sullo sviluppo delle tue applicazioni.

Un altro aspetto importante da considerare è la gestione delle date in locale. Gleam offre diversi modi per convertire le date tra fusi orari e formati, come `Iso` e `CalendarDate`. È importante comprendere la gestione delle date in locale per evitare problemi con offset e orari errati.

## Vedi anche

- [Documentazione della libreria `gleam/time`](https://gleam.run/docs/current/libraries/time)
- [Tutorial sulle date in Gleam](https://medium.com/runglish/calculating-dates-in-gleam-75d2c7cfb39e)
- [Esempi di codice per gestire le date in Gleam](https://github.com/gleam-lang/gleam/blob/master/lib/gleam-standard/lib/time.gleam)