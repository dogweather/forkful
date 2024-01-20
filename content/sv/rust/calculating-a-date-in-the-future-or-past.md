---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Rust: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller förflutna är processen att lägga till eller dra ifrån ett specifikt tidsspann från ett givet datum. Det används av programmerare för att hantera tid-relaterade logik i applikationer som databas sökning, händelseplanering m.m.

## Hur man gör:

Här är ett enkelt exempel som visar hur man kan räkna ut ett datum i framtiden med Rust.
```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let nuvarande_datum: DateTime<Utc> = Utc::now();
    println!("Nuvarande datum och tid: {}", nuvarande_datum);
    
    let framtida_datum: DateTime<Utc> = nuvarande_datum + Duration::days(30);
    println!("Framtida datum och tid: {}", framtida_datum);
}
```
För att beräkna ett datum i det förflutna kan vi dra av tidsintervallet från det aktuella datumet, på detta sätt:
```Rust
let tidigare_datum: DateTime<Utc> = nuvarande_datum - Duration::days(30);
println!("Tidigare datum och tid: {}", tidigare_datum);
```

## Djupare inblick

Historiskt sett, hade programmerare att räkna med komplexiteten hos tid och datum behandling, inklusive skottår, tidzoner och mycket mer. Rust förenklar detta genom `chrono`biblioteket, som tillhandahåller en mängd funktioner för att hantera tid och datum.

Alternativa sätt att beräkna datum i framtiden eller förflutna kan innefatta användning av andra Rust bibliotek som `time` eller `date`. Men `chrono` förblir att vara ett populärt val till följd av dess omfattande funktionalitet och enkelhet.

Implementationen av att beräkna ett datum i framtiden eller förflutna i Rust baseras på användningen av `Duration` för att representera ett tidsintervall, som sedan kan läggas till eller dras av från en `DateTime`.

## Se Även:

- Rust's officiella dokumentation: [chrono](https://docs.rs/chrono/0.4.0/chrono/index.html)
- Rust's officiella dokumentation: [Duration](https://doc.rust-lang.org/std/time/struct.Duration.html)
- Nybörjarguide till Rust: [Hantera datum och tid i Rust med Chrono](https://www.section.io/engineering-education/rust-date-and-time-manipulation-with-chrono/)