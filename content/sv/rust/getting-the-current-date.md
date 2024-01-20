---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför? 
Att hämta aktuellt datum är processen att programmera systemet att förstå det rådande datumet. Det används ofta inom programmering för att organisera och tidstämpla information.

## Så här gör du:
Rust erbjuder enkla verktyg att hämta det aktuella datumet. Använd `chrono`-biblioteket för att göra detta.

Installera det först genom att lägga till det i dina `Cargo.toml`-beroenden:

```Rust
[dependencies]
chrono = "0.4.19"
```

Kod för att hämta det nuvarande datumet:

```Rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let nu = Utc::now();
    println!("{}", nu);
}
```

Exempel på utsignal:

```Rust
2022-07-22 12:39:22.643892400 UTC
```

## Djupdykning 
Historiskt sett har behandling av datum och tid alltid varit en utmaning i programmering på grund av variationer i tidszoner och kalenderformat. Rust löste detta genom att införa `chrono`-biblioteket.

Alternativa sätt att hämta det nuvarande datumet kan vara genom att använda olika tid-bibliotek som `time` och `date-time`.

Implementationen med `chrono` hjälper till att hantera komplexiteten med tidszoner, vilket gör det till att föredras att använda för att hämta det nuvarande datumet.

## Se även
För mer information, se följande länkar:
- Chrono-biblioteket: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Rusts officiella dokumentation: [https://doc.rust-lang.org/stable/std/time/](https://doc.rust-lang.org/stable/std/time/)
- Översikt av tid och datum i Rust: [https://blog.thoughtram.io/time-and-dates/)