---
title:                "Ottenere la data attuale."
html_title:           "Rust: Ottenere la data attuale."
simple_title:         "Ottenere la data attuale."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Ottenere la data corrente è una cosa che i programmatori spesso fanno nelle loro applicazioni. Questo serve a tenere traccia del tempo e delle date in modo preciso all'interno di un programma.

## Come:

```Rust 
use std::time::SystemTime;
use chrono::{DateTime, Local};

let now: DateTime<Local> = DateTime::from(SystemTime::now());

println!("{}", now);
```

Output: 2021-10-25 10:00:00 

## Approfondimento:

Ottenere la data corrente è diventato particolarmente importante con l'avvento dei computer e l'evoluzione della tecnologia. In passato, quando i calcolatori erano meno diffusi, gli utenti dovevano impostare manualmente l'orologio del computer per tenerlo aggiornato con la data e l'ora corrente. Oggi, gli sviluppatori possono utilizzare le librerie di linguaggi di programmazione come Rust per ottenere facilmente la data e l'ora corrente nei loro programmi.

Altre alternative per ottenere la data corrente includono l'utilizzo di librerie di terze parti come chrono e time, che offrono ulteriori funzionalità per la gestione del tempo e delle date.

L'implementazione di base per ottenere la data corrente in Rust sfrutta la libreria standard std::time::SystemTime, che rappresenta l'istante corrente del sistema. Utilizzando la libreria chrono, l'oggetto SystemTime può essere convertito in un formato più leggibile come DateTime.

## Vedi anche:

- [Documentazione Chrono](https://docs.rs/chrono/latest/chrono/)
- [Documentazione Rust sulla gestione del tempo](https://doc.rust-lang.org/std/time/)
- [Tutorial su come ottenere la data corrente in Rust](https://www.freecodecamp.org/news/how-to-get-current-date-and-time-in-rust/)