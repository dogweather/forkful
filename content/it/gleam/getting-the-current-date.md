---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:14:30.025488-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Ottenere la data corrente nel programmazione significa catturare il momento presente in termini di giorno, mese e anno. I programmatori lo fanno per registrare eventi, scadenze, o semplicemente mostrare la data e l'ora agli utenti.

## Come Fare:
```gleam
import gleam/io
import gleam/calendar.{local_now, DateTime}

fn main() {
  let DateTime(year, month, day, _, _, _, _) = local_now()
  io.println("La data corrente è: " ++ int_to_string(day) ++ "/" ++ int_to_string(month) ++ "/" ++ int_to_string(year))
}
```
Output (esempio):
```
La data corrente è: 21/3/2023
```

## Approfondimento
Storicamente, ottenere la data corrente in programmazione è sempre stato essenziale per applicazioni e sistemi operativi. In Gleam, usiamo il modulo `gleam/calendar` per accedere alle funzioni legate al tempo e alla data. La funzione `local_now` restituisce un `DateTime` che possiamo destrutturare per ottenere i componenti che ci interessano, come anno, mese e giorno. Alternative a questo approccio includono l'utilizzo di librerie di terze parti o dirette chiamate a API del sistema operativo. Tieni presente che la gestione del tempo in programmazione può diventare complicata a causa del supporto per i fusi orari e le transizioni da ora solare a ora legale.

## Vedi Anche:
- Documentazione su `DateTime` in altre lingue di programmazione per confrontare l'approccio di Gleam.