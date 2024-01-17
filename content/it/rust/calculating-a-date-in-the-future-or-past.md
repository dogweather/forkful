---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Rust: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Calcolare una data nel futuro o nel passato è un'operazione comune per i programmatori. Si tratta di determinare una data basata su una data di partenza e un intervallo di tempo specificato, che può essere espresso in giorni, settimane, mesi o anni. I programmatori spesso eseguono questo tipo di calcolo per scopi di cronologia, pianificazione o per calcolare scadenze.

## Come fare:

```Rust
use chrono::{Utc, Duration, NaiveDate};

// Calcolo della data nel futuro
let today = Utc::today(); // data corrente
let future_date = today + Duration::days(20); // aggiungiamo 20 giorni

// Calcolo della data nel passato
let today = Utc::today(); // data corrente
let past_date = today - Duration::weeks(2); // sottraiamo 2 settimane
```

## Approfondimento:

Calcolare una data nel futuro o nel passato è una pratica comune nell'informatica fin dagli albori, quando i computer sono diventati in grado di tenere traccia del tempo. Esistono diverse alternative per eseguire questo tipo di calcolo, come l'utilizzo di librerie esterne o di funzioni integrate nelle librerie standard di un linguaggio di programmazione.

In Rust, la libreria "chrono" fornisce una serie di funzioni utili per gestire le date e i tempi, inclusa la possibilità di calcolare una data nel futuro o nel passato. Per calcolare una data in base a un intervallo di tempo specifico, questa libreria utilizza una struttura dati chiamata "Duration", che rappresenta l'intervallo di tempo in base a diversi parametri, come giorni, ore, minuti e secondi.

## Vedi anche:

Per ulteriori informazioni sulla libreria "chrono" e sul calcolo di date in Rust, consulta la documentazione ufficiale: https://docs.rs/chrono/latest/chrono/.

Puoi anche esplorare altre alternative, come la libreria "time" o la funzione "add" della libreria standard "std::time": https://doc.rust-lang.org/std/time/fn.add.html.