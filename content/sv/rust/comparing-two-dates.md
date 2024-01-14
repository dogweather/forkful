---
title:                "Rust: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har behövt jämföra två datum i ditt program eller applikation, är detta blogginlägg för dig! Att kunna jämföra datum effektivt är en viktig del av programmering och i denna artikel kommer vi att titta på hur man gör det på ett enkelt och effektivt sätt med hjälp av Rust.

## Hur man gör

Att jämföra två datum i Rust är ganska enkelt med hjälp av biblioteket "chrono". Först måste vi importera biblioteket i vår kod:

```Rust
extern crate chrono;
use chrono::{Datelike, NaiveDate};
```

Sedan kan vi skapa två datum som vi vill jämföra:

```Rust
let date1 = NaiveDate::from_ymd(2021, 1, 1);
let date2 = NaiveDate::from_ymd(2020, 12, 31);
```

Nu kan vi använda funktionen "cmp" för att jämföra de två datumen och få tillbaka resultatet som en "std::cmp::Ordering" enum:

```Rust
let result = date1.cmp(&date2);
println!("Resultat: {:?}", result);
```

Om de två datumen är lika kommer resultatet att vara "Equal". Om det första datumet är tidigare än det andra kommer resultatet att vara "Less" och om det första datumet är senare kommer resultatet att vara "Greater".

## Djupdykning

Vad händer egentligen bakom kulisserna när vi jämför två datum i Rust? Chrono-biblioteket konverterar datumen till en "NaiveDateTime" som består av antal sekunder sedan 1970-01-01 00:00:00 UTC. Sedan utförs jämförelsen baserad på detta antal sekunder.

Det är också viktigt att notera att "cmp" för närvarande inte stödjer att jämföra två datum med olika tidszoner. Detta kan dock lösas genom att först konvertera datumen till samma tidszon.

## Se även

- Chrono Documentation: https://docs.rs/chrono/0.4.19/chrono/
- Rust Standard Library Documentation: https://doc.rust-lang.org/std/