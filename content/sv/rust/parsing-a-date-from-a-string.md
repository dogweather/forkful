---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:38:41.789292-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Omformulering av datum från en sträng innebär att tolka text och konvertera den till en datumtyp programmerare kan jobba med. Detta är nödvändigt för att möjliggöra datumhantering, jämförelser och beräkningar.

## Hur man gör:
Rust har inget inbyggt för datumtolkning i standardbiblioteket, så vi använder `chrono`-paketet.

```Rust
extern crate chrono;
use chrono::{DateTime, Local, NaiveDateTime, TimeZone};

fn main() {
    // Skapa en NaiveDateTime från en sträng
    let date_string = "2023-03-14T13:07:00";
    let parsed_date = NaiveDateTime::parse_from_str(date_string, "%Y-%m-%dT%H:%M:%S")
        .expect("Fel format på datumsträngen");

    // Konvertera NaiveDateTime till DateTime<Local>
    let local_date: DateTime<Local> = Local.from_local_datetime(&parsed_date).unwrap();

    println!("Det tolkade datumet är: {}", local_date);
}
```

Resultatet visas som:
```
Det tolkade datumet är: 2023-03-14T13:07:00+01:00
```

## Djupdykning
Tidigare, programmerare var tvungna att tolka datumsträngar manuellt - en bökig och felbenägen process. `chrono` är den de facto standarden för datum och tid i Rust, fastän det är ett externt crate. Alternativ inkluderar egna tolkningsfunktioner eller andra crate.

`chrono` använder `NaiveDateTime` för datum och tid utan tidszonrepresentation, medan `DateTime<Tz>` representerar ett datum och en tid med en specifik tidszon där `Tz` är någon `TimeZone`. Att förstå skillnaden är centralt när datum och tid ska hanteras globalt.

## Se också
- Officiell `chrono` dokumentation: https://docs.rs/chrono/
- Rust's datum och tids API-design diskussioner: https://internals.rust-lang.org/t/proposal-a-new-date-and-time-library-for-rust/2495
- Tidszoner och lokalisering i Rust: https://blog.rust-lang.org/2020/01/27/Rust-1.41.0.html#whats-in-1.41.0-stable
