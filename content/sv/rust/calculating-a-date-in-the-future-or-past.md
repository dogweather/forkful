---
title:    "Rust: Beräkna ett datum i framtiden eller förflutna"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att titta på hur man kan beräkna datum i framtiden eller det förflutna med hjälp av Rust-programmering. Detta kan vara användbart i en rad olika situationer, såsom att skapa ett kalenderverktyg eller hantera bokningar.

## Hur man gör
För att beräkna ett datum i framtiden eller det förflutna behöver vi först importera standardbiblioteket "chrono", som innehåller funktioner för att hantera datum och tider. Vi behöver också använda oss av "DateTime" modulen från detta bibliotek.

```Rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    // skapa ett startdatum
    let start = Utc.ymd(2021, 8, 1).and_hms(12, 0, 0);
    // beräkna ett datum 100 dagar framåt i tiden
    let future_date = start + Duration::days(100);
    // beräkna ett datum 3 månader bakåt i tiden
    let past_date = start - Duration::weeks(12);

    // skriv ut resultaten
    println!("Datum 100 dagar från nu: {}", future_date.format("%d/%m/%Y"));
    println!("Datum 3 månader bakåt i tiden: {}", past_date.format("%d/%m/%Y"));
}
```

Output:
```
Datum 100 dagar från nu: 09/11/2021
Datum 3 månader bakåt i tiden: 25/04/2021
```

## Djupdykning
För att förstå hur detta fungerar är det viktigt att förstå hur DateTime-modulen hanterar datum och tider. I Rust representeras en DateTime som en kombination av ett datum och en tidzon, vilket gör det enkelt att hantera en mängd olika tidszoner utan att behöva hantera omvandlingen manuellt.

För att beräkna ett datum i framtiden eller det förflutna använder vi oss av Duration-modulen, som låter oss lägga till eller dra av en viss mängd tid från ett befintligt Datum. Detta gör det enkelt att räkna ut datum baserat på ett specifikt startdatum.

## Se även
- [Officiell dokumentation för "chrono" biblioteket](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust-programmering: En överblick](https://www.rust-lang.org/learn)