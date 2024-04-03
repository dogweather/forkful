---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:02.723870-07:00
description: "Hur man g\xF6r: #."
lastmod: '2024-03-13T22:44:37.708354-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:


### Använda Rusts Standardbibliotek (`chrono` Crate)
Rusts standardbibliotek inkluderar inte direkt datumtolkning, men den ofta använda `chrono`-craten är en robust lösning för datum- och tidsmanipulation. Lägg först till `chrono` i din `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Använd sedan `chrono` för att tolka en datumsträng till ett `NaiveDate`-objekt:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Misslyckades med att tolka datumet");

    println!("Tolkat datum: {}", date);
}

// Exempel på utskrift:
// Tolkat datum: 2023-04-01
```

### Använda Rusts Avancerade Datum-Tidshantering (`time` Crate)
För mer avancerad hantering av datum och tid, inklusive mer ergonomisk tolkning, överväg `time`-craten. Lägg först till den i din `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Tolka sedan en datumsträng med hjälp av `Date`-typen och `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Misslyckades med att tolka datum och tid");

    println!("Tolkat datetime: {}", parsed_date);
}

// Exempel på utskrift:
// Tolkat datetime: 2023-04-01 12:34:56
```

Båda exemplen visar hur Rust, med hjälp av tredjeparts-cratear, underlättar tolkningen av datumsträngar till manipulerbara datumobjekt, vilket gör det till ett kraftfullt verktyg för mjukvaruutveckling som innefattar temporala data.
