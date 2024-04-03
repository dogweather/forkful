---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:56.687492-07:00
description: "Att h\xE4mta det aktuella datumet i Rust \xE4r en vanlig uppgift f\xF6\
  r s\xE5dana saker som loggning, tidsbaserade operationer eller helt enkelt f\xF6\
  r att visa datumet.\u2026"
lastmod: '2024-03-13T22:44:37.709474-06:00'
model: gpt-4-0125-preview
summary: "Att h\xE4mta det aktuella datumet i Rust \xE4r en vanlig uppgift f\xF6r\
  \ s\xE5dana saker som loggning, tidsbaserade operationer eller helt enkelt f\xF6\
  r att visa datumet."
title: "F\xE5 det aktuella datumet"
weight: 29
---

## Vad & Varför?

Att hämta det aktuella datumet i Rust är en vanlig uppgift för sådana saker som loggning, tidsbaserade operationer eller helt enkelt för att visa datumet. Till skillnad från några språk som inkluderar datum- och tidsfunktionalitet i sitt standardbibliotek, uppmuntrar Rust användningen av ett robust tredjepartsbibliotek, chrono, för omfattande manipulation av datum och tid på grund av dess överlägsna funktionalitet och användarvänlighet.

## Hur man gör:

### Använda Rusts Standardbibliotek
Rusts standardbibliotek erbjuder ett begränsat men snabbt sätt att få den aktuella tiden, dock inte direkt det aktuella datumet i ett kalenderformat. Så här gör du:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Aktuell tid: {} sekunder sedan Unix epoken.", n.as_secs()),
        Err(_) => panic!("SystemTid före Unix epoken!"),
    }
}
```

Utdata:
```
Aktuell tid: 1615390665 sekunder sedan Unix epoken.
```

### Använda Chrono-biblioteket
För mer omfattande datum- och tidsfunktionalitet, inklusive att få det aktuella datumet, bör du använda `chrono`-biblioteket. Lägg först till `chrono` i din `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Sedan kan du använda `chrono` för att få det aktuella datumet:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let nu = Local::now();
    println!("Aktuella datumet: {}-{}-{}", nu.year(), nu.month(), nu.day());
}
```

Utdata:
```
Aktuella datumet: 2023-4-20
```

`chrono`-biblioteket gör det enkelt att arbeta med datum och tider, och erbjuder en mängd funktionaliteter utöver att bara hämta det aktuella datumet, inklusive att tolka, formatera och utföra aritmetiska operationer på datum och tider.
