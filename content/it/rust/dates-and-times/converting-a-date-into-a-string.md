---
aliases:
- /it/rust/converting-a-date-into-a-string/
date: 2024-01-20 17:37:32.570042-07:00
description: "Convertire una data in una stringa significa rappresentare la data come\
  \ testo. Lo facciamo perch\xE9 \xE8 pi\xF9 facile da leggere per gli umani e perch\xE9\
  \ \xE8 un\u2026"
lastmod: 2024-02-18 23:08:55.691430
model: gpt-4-1106-preview
summary: "Convertire una data in una stringa significa rappresentare la data come\
  \ testo. Lo facciamo perch\xE9 \xE8 pi\xF9 facile da leggere per gli umani e perch\xE9\
  \ \xE8 un\u2026"
title: Conversione di una data in una stringa
---

{{< edit_this_page >}}

## What & Why?
Convertire una data in una stringa significa rappresentare la data come testo. Lo facciamo perché è più facile da leggere per gli umani e perché è un formato comune per memorizzare o trasmettere dati.

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
