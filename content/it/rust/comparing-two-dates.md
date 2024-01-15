---
title:                "Confrontare due date"
html_title:           "Rust: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se ti sei mai trovato a dover confrontare due date in un programma Rust, allora sai quanto possa essere complicato gestire tutti i dettagli dei diversi formati e timezone. In questo articolo vedremo come semplificare questo processo utilizzando le funzioni di data di Rust.

## Come fare

Per confrontare due date, abbiamo bisogno di convertirle in un formato comune e facile da confrontare, come ad esempio il formato Unix timestamp. Utilizzando la libreria standard di Rust, possiamo ottenere il timestamp di una data utilizzando il metodo `.timestamp()` su un oggetto `DateTime`:

```Rust
use chrono::{DateTime, Utc};
let prima_data = DateTime::parse_from_rfc3339("2021-04-01T12:00:00Z").unwrap();
let secondo_data = DateTime::parse_from_rfc3339("2021-04-02T12:00:00Z").unwrap();
let prima_timestamp = prima_data.timestamp();
let secondo_timestamp = secondo_data.timestamp();
```

Una volta ottenuti i due timestamp, possiamo semplicemente confrontarli utilizzando gli operatori di confronto standard:

```Rust
if prima_timestamp < secondo_timestamp {
    println!("La prima data è prima della seconda data");
} else if prima_timestamp > secondo_timestamp {
    println!("La prima data è dopo la seconda data");
} else {
    println!("Le due date sono uguali");
}
```

Questo è solo un semplice esempio di come si possano confrontare due date, ma può essere facilmente adattato per soddisfare le proprie esigenze specifiche.

## Approfondimento

Se vogliamo gestire in modo più accurato le date, possiamo utilizzare le funzioni della libreria `chrono` per ottenere informazioni più dettagliate, come ad esempio il giorno, il mese o l'anno di una data specifica. Possiamo anche utilizzare il metodo `.with_timezone()` per convertire una data in un determinato fuso orario prima di ottenerne il timestamp.

Per ulteriori informazioni sulle funzioni di data di Rust, si consiglia di consultare la documentazione della libreria `chrono` e di ulteriori risorse online.

## Vedi anche

- [Documentazione della libreria `chrono`](https://docs.rs/chrono/latest/chrono/)
- [Esempi di utilizzo delle funzioni di data di Rust](https://rust-lang-nursery.github.io/rust-cookbook/datetime.html)