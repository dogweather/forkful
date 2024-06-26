---
date: 2024-01-20 17:37:32.570042-07:00
description: "How to: Per convertire una data in una stringa in Rust, possiamo usare\
  \ le funzionalit\xE0 di chrono, una crate esterna."
lastmod: '2024-03-13T22:44:43.229941-06:00'
model: gpt-4-1106-preview
summary: "Per convertire una data in una stringa in Rust, possiamo usare le funzionalit\xE0\
  \ di chrono, una crate esterna."
title: Conversione di una data in una stringa
weight: 28
---

## How to:
Per convertire una data in una stringa in Rust, possiamo usare le funzionalità di chrono, una crate esterna:

```Rust
extern crate chrono;
use chrono::{DateTime, Local, TimeZone};

fn main() {
    // Ottieni la data e ora corrente
    let now: DateTime<Local> = Local::now();
    
    // Converti in una stringa nel formato desiderato
    let date_string = now.format("%Y-%m-%d %H:%M:%S").to_string();
    
    // Stampa la data convertita
    println!("Data e ora correnti: {}", date_string);
}
```

Output:
```
Data e ora correnti: 2023-04-05 14:30:21
```

## Deep Dive
In Rust, la serializzazione delle date viene gestita in modo robusto dalla crate `chrono`. Prima di `chrono`, la manipolazione delle date era un po' rudimentale e limitata; `chrono` ha portato una vasta gamma di funzionalità e formattazioni per date e orari, ispirandosi ampiamente a librerie come `DateTime` in C# e `moment.js` in JavaScript.

Oltre a `chrono`, Rust offre alcune alternative come `time` o l'uso del modulo `SystemTime` nella crate standard. Tuttavia, `chrono` è spesso preferito per la sua ricchezza di funzionalità e facilità d'uso.

Convertire una data in una stringa coinvolge normalmente la specificazione di un formato. Rust fa uso degli specifiers di formato, simili a quelli in C e nell'ISO C++. Quindi `%Y-%m-%d %H:%M:%S` è un modo comune per rappresentare l'anno, il mese, il giorno seguito dall'orario in formato 24 ore.

## See Also
- Rust Documentation: https://doc.rust-lang.org/std/time/
- Chrono Crate Documentation: https://docs.rs/chrono/
- The 'time' Crate: https://docs.rs/time/
- Rust Cookbook Date/Time Example: https://rust-lang-nursery.github.io/rust-cookbook/datetime.html
