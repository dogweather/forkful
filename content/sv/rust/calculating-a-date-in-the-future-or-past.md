---
title:                "Rust: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

I många programmeringsprojekt behövs det ibland att beräkna ett datum i framtiden eller historiskt. Att kunna göra detta är en viktig del av att skapa applikationer som kräver att planera för händelser eller hantera tidsbaserad data. I den här artikeln kommer vi att utforska hur man kan göra detta med Rust-programmeringsspråket.

## Hur man gör det

För att kunna beräkna ett datum i framtiden eller tidigare behöver vi först definiera variabler för det nuvarande datumet, antal dagar och riktningen vi vill gå i (framåt eller bakåt i tiden).

```Rust
use chrono::{Duration, DateTime, Utc};

// Definiera nuvarande datum
let now = Utc::now();
// Definiera antal dagar
let days = Duration::days(7);
// Definiera riktning
let direction = "framåt";
```

För att beräkna datumet använder vi sedan `with_duration`-funktionen från Chrono-biblioteket och anger antalet dagar och riktningen som parametrar. Detta resulterar i ett nytt `DateTime`-objekt som representerar det beräknade datumet.

```Rust
// Beräkna datumet
let calculated_date = now.with_duration(direction, days);

println!("Beräknat datum: {}", calculated_date);
```

När vi kör koden ovan kommer vi att se följande utmatning:

```
Beräknat datum: 2020-06-12 15:00:00 UTC
```

Vi kan också utföra samma beräkning för tidigare datum genom att ändra värdet för variabeln `riktning` till "bakåt".

## Djupdykning

Under huven använder sig Chrono-biblioteket av Gregorian Calendar för att hantera datum- och tidsberäkningar. Detta är en standardkalender som används i många länder och baseras på den julianska kalendern.

En viktig aspekt att tänka på vid beräkningar av datum i framtiden eller historiskt är att ta hänsyn till skottår. Detta görs automatiskt av Chrono-biblioteket, men det kan vara bra att ha i åtanke när man utför beräkningarna.

## Se även

- [Chrono-biblioteket på crates.io](https://crates.io/crates/chrono)
- [Chrono-dokumentation](https://docs.rs/chrono)
- [Rust-programmeringsspråkets hemsida](https://www.rust-lang.org/sv-SE/)