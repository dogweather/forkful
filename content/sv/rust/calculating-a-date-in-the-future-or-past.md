---
title:                "Rust: Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara väldigt användbart i många situationer, till exempel vid schemaläggning eller för att hålla koll på kommande händelser.

## Så här gör du

Det finns många olika sätt att beräkna ett datum i framtiden eller det förflutna i Rust, men vi kommer att fokusera på en enkel metod. Först behöver du importera modulen `chrono`, som innehåller funktioner för att hantera datum och tid.

```
extern crate chrono;
use chrono::prelude::*;
```

För att beräkna ett datum i framtiden eller det förflutna kan du använda funktionen `naive_date` och ange antalet dagar som ska adderas eller subtraheras från ett specifikt datum.

```
let current_date = NaiveDate::from_ymd(2021, 11, 3);
let future_date = current_date.naive_date().unwrap() + Duration::days(45);
let past_date = current_date.naive_date().unwrap() - Duration::days(10);

println!("Detta är datumet 45 dagar från nu: {}", future_date);
println!("Detta är datumet 10 dagar före nu: {}", past_date);
```

Denna kod kommer att ge följande utdata:

```
Detta är datumet 45 dagar från nu: 2021-12-18
Detta är datumet 10 dagar före nu: 2021-10-24
```

## Djupdykning

För att förstå hur detta fungerar behöver vi först förstå vad `naive_date` och `Duration` gör. Funktionen `naive_date` skapar en nytt `NaiveDate`-objekt från ett specifikt datum, medan `Duration` är en strukt som kan användas för att representera en tidsintervall. Genom att lägga till eller subtrahera ett `Duration`-objekt från ett `NaiveDate`-objekt kan vi enkelt beräkna ett nytt datum.

Det är också värt att nämna att `chrono`-modulen innehåller många andra användbara funktioner för att hantera datum och tid. Det är definitivt värt att djupdyka i dokumentationen för att lära sig mer.

## Se även

- [Dokumentation för chrono-modulen](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Rust standardbibliotekets dokumentation för datum och tid](https://doc.rust-lang.org/std/time/index.html)