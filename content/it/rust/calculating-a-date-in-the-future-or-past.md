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

# Calcolo delle Date Futuristiche o Passate in Rust

## Cos'è e Perché? 
Calcolare una data nel futuro o nel passato è il processo di aggiungere o sottrarre un certo numero di giorni, mesi o anni da una data specifica. Questo è utile per i programmatori per gestire funzioni come i timer, la programmazione e le funzioni legate ai promemoria.

## Come si Fa?
In Rust, si può utilizzare la libreria Chrono per gestire le date. Ecco un esempio semplice:

```Rust
use chrono::{Date, Utc, Duration};

// Creiamo una data di riferimento: 24 dicembre 2022.
let now: Date<Utc> = Utc.ymd(2022, 12, 24);

// Data nel futuro: 30 giorni dopo la data di riferimento.
let future: Date<Utc> = now + Duration::days(30);
println!("Data futura: {}", future);

// Data nel passato: 15 giorni prima della data di riferimento.
let past: Date<Utc> = now - Duration::days(15);
println!("Data passata: {}", past);
```

L'esecuzione di questo codice restituirà qualcosa di simile a:

```Rust
Data futura: 2023-01-23UTC
Data passata: 2022-12-09UTC
```

## Approfondimento
* ### Contesto Storico
La gestione delle date nel campo della programmazione è sempre stata una fonte di problemi. Dall'Y2K al bug del 2038, i problemi relativi alle date hanno causato notevoli grattacapi. Rust, con la sua libreria Chrono, semplifica notevolmente questi processi.

* ### Alternative
Esistono altre librerie, come Time, che offre funzionalità simili a Chrono. Il motivo per cui preferiamo Chrono è la sua sintassi intuitiva e il supporto nativo per le operazioni tra date.

* ### Dettagli Implementativi
Le operazioni sulle date in Chrono ruotano intorno a `Duration`, che rappresenta un periodo. Si può facilmente sommare o sottrarre i periodi da `Date` per calcolare date future o passate. 

## Vedi Anche
Per saperne di più sulla gestione delle date in Rust, dai un'occhiata a questi link:
- [Documentazione Chrono](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Guida del libro Rust](https://doc.rust-lang.org/book/)
- [Discussione su StackOverflow](https://stackoverflow.com/questions/27338792/calculate-time-difference-in-rust) sul calcolo della differenza temporale in Rust.