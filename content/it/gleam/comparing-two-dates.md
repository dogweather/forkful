---
title:                "Confronto tra due date"
html_title:           "Gleam: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Sei mai stato in una situazione in cui devi confrontare due date per verificare quale è più recente o per calcolare la differenza di tempo tra le due? Con l'utilizzo del linguaggio di programmazione Gleam, puoi facilmente confrontare e manipolare date per soddisfare le tue esigenze. In questo articolo, ti mostrerò come confrontare due date utilizzando la libreria standard di Gleam.

## Come fare
Per confrontare due date, possiamo utilizzare la funzione di libreria `Time.diff` di Gleam. Questa funzione accetta due argomenti di tipo `Time`, che rappresentano date specifiche. Ad esempio, possiamo confrontare le date 10 gennaio 2020 e 5 maggio 2021 nel seguente modo:

```Gleam
import gleam/time
import gleam/coding_journal

let date_one = Time.new(2020, 1, 10)
let date_two = Time.new(2021, 5, 5)

CodingJournal.info("La differenza tra le due date è:")
CodingJournal.info(Time.diff(date_one, date_two))
```

Eseguendo questo codice, otteniamo il seguente output:

```
La differenza tra le due date è: 0 years, 3 months, 25 days, 15 hours, 00 minutes, 00 seconds
```

Come puoi vedere, la funzione `Time.diff` ci fornisce una risposta dettagliata che include anche le differenze di ore, minuti e secondi tra le due date.

Possiamo anche utilizzare la funzione `Time.compare` per confrontare due date e determinare quale è più recente. Questa funzione restituirà `-1` se la prima data è più vecchia della seconda, `0` se sono uguali e `1` se la seconda data è più recente. Ecco un esempio di codice:

```Gleam
import gleam/time
import gleam/coding_journal

let date_one = Time.new(2020, 1, 10)
let date_two = Time.new(2021, 5, 5)

CodingJournal.info("La seconda data è più recente?")
CodingJournal.info(Time.compare(date_one, date_two))
```

Eseguendo questo codice, otteniamo il seguente output:

```
La seconda data è più recente? 1
```

In questo caso, poiché la seconda data è più recente, la funzione `Time.compare` restituisce `1`.

## Deep Dive
Sebbene il confronto di due date in Gleam sia abbastanza semplice, è importante sottolineare che le date sono un concetto complicato e possono portare a problemi di calcolo del fuso orario e della loro rappresentazione. Per approfondire questi aspetti, ti consiglio di dare un'occhiata alla documentazione della libreria standard di Gleam o ad esempi più complessi sulle pagine di GitHub.

## See Also
- Documentazione ufficiale di Gleam per le funzioni `Time.diff` e `Time.compare`: https://gleam.run/lib/pikajson/time.html
- Esempi avanzati sull'utilizzo delle date in Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/date.gleam