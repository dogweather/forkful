---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:20.690532-07:00
description: "Come fare: La libreria standard di Rust non include direttamente l'analisi\
  \ delle date, ma la crate `chrono`, ampiamente utilizzata, \xE8 una soluzione\u2026"
lastmod: '2024-03-13T22:44:43.227483-06:00'
model: gpt-4-0125-preview
summary: "La libreria standard di Rust non include direttamente l'analisi delle date,\
  \ ma la crate `chrono`, ampiamente utilizzata, \xE8 una soluzione robusta per la\
  \ manipolazione di date e orari."
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:


### Utilizzando la Libreria Standard di Rust (`chrono` Crate)
La libreria standard di Rust non include direttamente l'analisi delle date, ma la crate `chrono`, ampiamente utilizzata, è una soluzione robusta per la manipolazione di date e orari. Prima di tutto, aggiungi `chrono` al tuo `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Quindi, utilizza `chrono` per analizzare una stringa rappresentante una data in un oggetto `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Errore nell'analisi della data");

    println!("Data analizzata: {}", date);
}

// Esempio di Output:
// Data analizzata: 2023-04-01
```

### Utilizzando la Gestione Avanzata di Date e Ora di Rust (`time` Crate)
Per una gestione più avanzata di date e orari, inclusa un'analisi più ergonomica, considera la crate `time`. Prima di tutto, includila nel tuo `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Quindi, analizza una stringa rappresentante una data usando il tipo `Date` and `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Errore nell'analisi della data e dell'ora");

    println!("Data e ora analizzate: {}", parsed_date);
}

// Esempio di Output:
// Data e ora analizzate: 2023-04-01 12:34:56
```

Entrambi gli esempi mostrano come Rust, con l'aiuto delle crate di terze parti, facilita l'analisi delle stringhe delle date in oggetti di data manipolabili, rendendolo uno strumento potente per lo sviluppo di software che coinvolge dati temporali.
