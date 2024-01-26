---
title:                "Calcolo di una data futura o passata"
date:                  2024-01-20T17:32:00.589273-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calcolo di una data futura o passata"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare una data nel futuro o nel passato significa trovare una data che è un certo numero di giorni, mesi o anni distante da una data di partenza. I programmatori lo fanno per gestire scadenze, eventi futuri, o per calcolare periodi passati.

## How to:
Rust usa il crate `chrono` per la gestione delle date. Ecco un esempio di come calcolare date nel futuro e nel passato.

```rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();

    let future_date = now + Duration::days(30);
    let past_date = now - Duration::days(30);

    println!("Data attuale: {}", now);
    println!("Data futura: {}", future_date);
    println!("Data passata: {}", past_date);
}
```

Output:
```
Data attuale: 2023-04-12T15:30:45.123456789Z
Data futura: 2023-05-12T15:30:45.123456789Z
Data passata: 2023-03-13T15:30:45.123456789Z
```

## Deep Dive
Calcolare date future o passate è un'esigenza comune. Prima del crate `chrono`, Rust usava tipi di tempo `std::time`. `chrono` è più flessibile e facile da usare per date e orari. Altre lingue usano librerie simili, come Joda-Time in Java. `chrono` rende semplice implementare l'aritmetica delle date usando `Duration`, che gestisce automaticamente la complessità, come anni bisestili e la longitudine dei mesi.

## See Also
Per esplorare di più su `chrono` e sulla gestione delle date in Rust, guarda questi link:
- Documentazione di `chrono`: https://docs.rs/chrono/
- Rust by Example su date e orari: https://doc.rust-lang.org/rust-by-example/std_misc/chrono.html
- Blog di Rust sul trattamento delle date e del tempo: https://blog.rust-lang.org/2020/05/07/working-with-time-in-Rust.html
