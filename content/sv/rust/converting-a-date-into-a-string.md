---
title:                "Omvandla ett datum till en sträng"
aliases:
- sv/rust/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:37.522960-07:00
model:                 gpt-4-1106-preview
simple_title:         "Omvandla ett datum till en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng innebär att omvandla datumdata till en textrepresentation. Programmerare gör detta för att underlätta visning och lagring i textbaserade format som JSON eller CSV.

## Så här gör du:
```rust
use chrono::{DateTime, Utc, TimeZone};

fn main() {
    let now: DateTime<Utc> = Utc::now(); // Nuvarande datum och tid i UTC
    println!("{}", now.to_string()); // Standardkonvertering till sträng
    
    // Anpassad format
    println!("{}", now.format("%Y-%m-%d %H:%M:%S").to_string());
}

// Exempel på utskrift:
// 2023-04-05T14:30:12Z
// 2023-04-05 14:30:12
```

## Djupdykning
Historiskt har datumhantering i de flesta programmeringsspråk utvecklats från enkla tidsstämplar till rika bibliotek som hanterar tidzoner och kalenderfunktioner. Rust använder ofta `chrono`-krate för datum och tid, som erbjuder funktioner för att konvertera och formatera datum. Alternativ är Rusts standardbibliotekets `time` modul, men den är mindre kraftfull.

Vid implementering måste man hantera tidzoner, lokalisering och olika datumformat. `format`-metoden i `chrono` låter oss definiera exakt hur datumet ska se ut som en sträng genom att använda formatkoder, som `%Y-%m-%d %H:%M:%S` för det vanliga åååå-mm-dd hh:mm:ss formatet.

## Se Mer
- `chrono` krate på crates.io: https://crates.io/crates/chrono
- Rust-dokumentation för tidsformatering: https://doc.rust-lang.org/std/time/index.html
- strftime formatteringskoder: https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html
