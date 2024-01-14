---
title:    "Gleam: Ottenere la data odierna"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Può sembrare banale, ma sapere la data corrente è un'informazione importante per molte applicazioni. Ad esempio, può essere utilizzata per gestire le scadenze dei pagamenti, creare un diario digitale o semplicemente visualizzare la data corrente per l'utente.

## Come Fare

Per ottenere la data corrente in Gleam, possiamo utilizzare il modulo ```gleam/time``` e la funzione ```now()```. Questo ci darà un record con il fuso orario, l'ora, i minuti, i secondi e i millisecondi correnti.

```Gleam
import gleam/time

let now = time.now()

// Output:
// Date(
//   time_zone: Timezone.Etc,
//   hour: 10,
//   min: 25,
//   sec: 5,
//   msec: 789,
// )
```

Se vogliamo ottenere la data attuale senza il fuso orario, possiamo utilizzare la funzione ```utc_now()```, che ci darà lo stesso risultato ma con il fuso orario impostato su UTC.

```Gleam
import gleam/time

let now = time.utc_now()

// Output:
// Date(
//   time_zone: Timezone.UTC,
//   hour: 10,
//   min: 25,
//   sec: 5,
//   msec: 789,
// )
```

## Approfondimento

La funzione ```now()``` e ```utc_now()``` utilizzano il modulo ```gleam/time``` e il suo sottotipo ```Date``` per rappresentare la data corrente. Questa struttura dati è immutabile e offre diversi metodi utili per manipolare la data, come ad esempio ```add_days()``` per aggiungere un numero di giorni alla data corrente.

Se si desidera ottenere la data in un formato diverso, è possibile utilizzare la funzione ```format()``` che accetta una stringa di formato e restituisce una stringa con la data formattata.

```Gleam
import gleam/time

let now = time.utc_now()
let formatted_date = time.format(now, "%d/%m/%Y")

// Output:
// 05/08/2021
```

## Vedi Anche

- Documentazione ufficiale di Gleam sul modulo ```gleam/time```: https://gleam.run/modules/time
- Esempi di utilizzo del modulo ```gleam/time```: https://github.com/gleam-lang/gleam_stdlib/blob/main/std/time_tests