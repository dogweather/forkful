---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Confrontare due date significa stabilire quale data viene prima e quale dopo. I programmatori lo fanno per organizzare eventi, stabilire scadenze o gestire cicli di vita del software.

## Come fare:
Ecco un esempio di confronto tra due date in Gleam.

```Gleam
import gleam/datetime.{day, from_iso_string, iso_year_month_day}
import gleam/compare.{less_than}

let giorno_uno = from_iso_string("2022-12-12").unwrap()
let giorno_due = from_iso_string("2022-12-13").unwrap()

let confronto = 
    case less_than(day(giorno_uno), day(giorno_due)) {
      False -> "La Data Uno non è anteriore alla Data Due."
      True -> "La Data Uno è anteriore alla Data Due."
}
```

Il programma di sopra confronta se il `giorno_uno` (2022-12-12) è minore del `giorno_due` (2022-12-13). L'output sarà "La Data Uno è anteriore alla Data Due."

## Approfondimento
Storicamente, confrontare le date non è stato sempre così semplice. Nel passato, l'implementazione del confronto delle date variava a seconda del linguaggio di programmazione.

Gleam offre un'alternativa più sicura utilizzando tipi fortemente tipizzati e pattern matching, eliminando errori comuni come i null pointer exceptions.

Il confronto delle date in Gleam avviene attraverso la funzione `less_than`, la quale restituisce un valore booleano in base al confronto tra i giorni delle due date passate como parametro.

## Vedere anche:
Consulta le seguenti fonti per una comprensione più approfondita del confronto delle date in Gleam:
- *[Gleam Docs - Date and Time](https://gleam.run/book/tour/dates-and-times.html)*
- *[Gleam DateTime module Github](https://github.com/gleam-lang/stdlib/tree/main/src/gleam/datetime.gleam)*
- *[Gleam Compare module Github](https://github.com/gleam-lang/stdlib/tree/main/src/gleam/compare.gleam)*