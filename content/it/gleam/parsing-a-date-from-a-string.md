---
title:                "Estrarre una data da una stringa"
html_title:           "Gleam: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Con "parsing di una data da una stringa" si intende convertire una stringa di testo contenente una data in un formato utilizzabile per il calcolo e la visualizzazione delle informazioni della data. I programmatori utilizzano questa tecnica per gestire dati temporali in modo efficiente e preciso.

## Come fare:
L'ultima versione di Gleam ti permette di eseguire il parsing di una data da una stringa in modo semplice e preciso. Ecco un esempio di codice:

```Gleam
import gleam/date

let parse_result = date.parse("17/05/2021", "dd/MM/yyyy")
```

Il risultato sarà un record di tipo `ParseResult` contenente l'oggetto `date` con i valori estratti dalla stringa di input. Ecco un esempio di output:

```Gleam
ParseResult(
  Ok(
    date(
      2021,
      date.May,
      17
    )
  )
)
```

## Deep Dive:
Il parsing delle date da una stringa è un'operazione comune nella programmazione, utilizzata per la gestione di orari, scadenze e altre informazioni temporali. In passato, questa operazione richiedeva l'utilizzo di librerie esterne, ma con Gleam puoi farlo nativamente, senza dipendenze aggiuntive. Se desideri approfondire ulteriormente, puoi anche esplorare altre alternative come le espressioni regolari o la libreria `chrono`. In termini di implementazione, Gleam utilizza uno schema di parsing flessibile basato su uno string buffer e una serie di caratteri di formattazione predefiniti.

## See Also:
- Documentazione ufficiale Gleam: https://gleam.run/
- Tutorial su come utilizzare il parsing delle date: https://gleam.run/book/tutorials/using-date-parsing