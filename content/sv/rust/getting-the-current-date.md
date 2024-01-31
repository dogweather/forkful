---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:16:37.460916-07:00
simple_title:         "Att hämta aktuellt datum"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hämta aktuellt datum betyder att du får tag på dagens datum i programmet. Programmerare gör det för att loggföra, för funktioner som är tidsberoende, eller helt enkelt för att visa datumet för användaren.

## Hur man gör:

```Rust
use chrono::{Local, Datelike};

fn main() {
    let nu = Local::now();
    println!("Idag är det {}-{}-{}", nu.year(), nu.month(), nu.day());
}
```
Exempel på output:
```
Idag är det 2023-04-07
```

## Djupdykning

Att hämta aktuellt datum är inget nytt i programmeringsvärlden, men sättet vi gör det på har förändrats över tid. I Rust används ofta `chrono`-paketet för att hantera datum och tid eftersom det inte finns något inbyggt i standardbiblioteket som hanterar detta väl.

Alternativ till `chrono` kan vara att använda `time`-biblioteket eller, om du har mer specifika behov, att använda plattformsspecifika funktioner genom FFI (Foreign Function Interface).

Detaljerna i att implementera datumhantering involverar tidszoner, skottår och formattering. `chrono` hanterar detta elegant, och metoder som `.year()`, `.month()`, och `.day()` returnerar helt enkelt aktuella värden baserade på systemets lokala tid.

## Se också

- The `chrono` crate documentation: https://docs.rs/chrono/
- The `time` crate as an alternative: https://docs.rs/time/
- Rust FFI for interacting with other languages: https://doc.rust-lang.org/nomicon/ffi.html
