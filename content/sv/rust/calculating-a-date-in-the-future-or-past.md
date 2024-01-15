---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Rust: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift inom programmering, vare sig det handlar om att skapa en kalenderapplikation eller hantera datum i databaser. Med Rusts robusta och snabba funktioner för datumberäkning, kan man enkelt och säkert hantera sådana uppgifter.

## Så här gör du

För att beräkna datum i Rust, behöver vi använda oss av standardbiblioteket `chrono`. För att komma igång, behöver vi först importera biblioteket genom att lägga till följande i vår kod:

```Rust
extern crate chrono;
```

Sedan kan vi använda oss av olika funktioner inom `chrono` för att beräkna datum. Nedan följer några exempel på hur man kan använda dessa funktioner:

- För att få dagens datum och tid:

```Rust
use chrono::offset::Local;

let today = Local::now();
```

- För att få datumet 10 dagar framåt:

```Rust
use chrono::{Duration, Local};

let future_date = Local::now() + Duration::days(10);
```

- För att få datumet 2 månader bakåt:

```Rust
use chrono::offset::Local;

let past_date = Local::now().month_subtract(2);
```

- För att konvertera ett datum till en viss tidszon:

```Rust
use chrono::prelude::*;

let timezone_jp = FixedOffset::east(9 * 3600); // Japan tidszon
let now = Utc::now(); // Omvandlar till UTC-tidszon
let tokyo_now = now.with_timezone(&timezone_jp); // Omvandlar till Tokyo tidszon
```

## Deep Dive

När vi beräknar datum i Rust, är det viktigt att förstå skillnaden mellan lokala och globala tidszoner. När vi använder funktioner som `Local::now()`, tar den hänsyn till den lokala tidszonen där koden körs. Medan funktioner som `Utc::now()` använder den globala tidszonen UTC (Coordinated Universal Time).

`chrono` har också olika strukturer för att hantera datum och tid, som `NaiveDateTime` och `DateTime`. Det är viktigt att vara medveten om vilken struktur man använder beroende på vad man vill uppnå.

## Se också

- [chrono hemsida](https://docs.rs/chrono/0.4.9/chrono/)

- [Datum- och tidsmanipulering i Rust](https://rust-lang-nursery.github.io/rust-cookbook/datetime/durations.html)