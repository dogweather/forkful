---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:16:26.709354-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente è una funzione che ti consente di scoprire quale giorno è oggi nel formato che desideri. I programmatori la usano per log, scadenze, features legate al tempo e per tenere traccia degli eventi.

## How to:

Per ottenere la data corrente in Rust, devi usare il crate `chrono`. Aggiungi `chrono` come dipendenza nel tuo file `Cargo.toml`. Ecco un esempio:

```Rust
// Aggiungi nel tuo Cargo.toml: chrono = "0.4"

extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let oggi = Local::now();
    println!("Data corrente: {}", oggi.format("%d/%m/%Y").to_string());
}
```

Output di esempio:
```
Data corrente: 28/03/2023
```

## Deep Dive

Alla base del sistema di gestione delle date e del tempo in Rust c’è il crate `chrono`. Prima di `chrono`, i programmatori usavano il modulo `time` della libreria standard, ma `chrono` ha portato maggiore precisions e funzioni.

`chrono` offre diversi tipi di data e ora, come `NaiveDateTime` per l'ora UTC e `DateTime` per il tempo con fuso orario. L’uso di `Local::now()` restituisce l’istante attuale nel fuso orario locale. Ci sono anche altre funzioni, come `Utc::now()` per l'ora UTC.

Per formati di data personalizzati, `chrono` si appoggia sulle specifiche del C `strftime` per un controllo granulare della formattazione della data.

## See Also

- Documentazione `chrono`: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Repo GitHub `chrono`: [https://github.com/chronotope/chrono](https://github.com/chronotope/chrono)
- Formattazione datari `strftime` in Rust: [https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)
