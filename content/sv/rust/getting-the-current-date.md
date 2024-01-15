---
title:                "Att hämta dagens datum"
html_title:           "Rust: Att hämta dagens datum"
simple_title:         "Att hämta dagens datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumet kan vara väldigt användbart i många olika programmeringsprojekt. Det kan hjälpa till med att skapa tidsstämplar, lägga till datum i filnamn eller till och med för att hålla koll på när en viss kod skrevs.

## Hur man gör det

För att få den nuvarande datumet i Rust, måste vi lägga till biblioteket `chrono` till vårt program. Sedan kan vi använda funktionen `Local::today()` för att få den nuvarande datumet. Här är ett exempel på kod och tillhörande utmatning:

```Rust
use chrono::prelude::*;

fn main() {
  let today = Local::today();
  println!("Den nuvarande datumet är: {}", today);
}
```

Output:

```
Den nuvarande datumet är: 2021-12-09
```

Om vi vill ha datumet i ett annat format såsom `DD-MM-YYYY` så kan vi använda funktionen `format()` tillsammans med en formaterare. Här är ett annat exempel på kod som visar detta:

```Rust
use chrono::prelude::*;

fn main() {
  let today = Local::today();
  let formatted_date = today.format("%d-%m-%Y").to_string();
  println!("Datumet idag är: {}", formatted_date);
}
```

Output:

```
Datumet idag är: 09-12-2021
```

## Djupdykning

Bakom kulisserna använder `Local::today()` funktionen faktiskt en annan funktion som heter `now()` från `chrono` biblioteket. Detta hämtar tiden som `chrono::DateTime` strukturerad data och sedan extraherar bara datumet från det. Det är enkelt och effektivt att använda, men om du vill ha mer kontroll över datumet kan du också använda `DateTime::from_utc()` eller `DateTime::from_local()`. Om du vill ha en komplett förståelse för hur `chrono` biblioteket fungerar, kan du läsa mer på dess dokumentation.

## Se även

- [Chrono dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Datum och tidshantering i Rust med Chrono](https://blog.logrocket.com/date-and-time-handling-in-rust-with-chrono/)
- [Rust Language Cheat Sheet](https://cheats.rs/)