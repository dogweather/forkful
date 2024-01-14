---
title:                "Rust: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den aktuella datumet är en viktig del av många olika program som utvecklas i Rust. Det kan användas för att hålla reda på tidsstämplar, planera framtida händelser eller helt enkelt för att visa aktuell tid på en användargränssnitt. Oavsett vad ditt projekt behöver, är det viktigt att veta hur man får tag på den aktuella datumet i Rust.

## Så här gör du

För att få tag på den aktuella datumet i Rust, används den inbyggda standardbiblioteket `chrono`. Genom att importera detta bibliotek kan vi enkelt skapa ett datum-objekt och få access till dess egenskaper.

```Rust
use chrono::{Local, DateTime};

let nu = Local::now(); // skapar ett DateTime objekt för nuvarande tid
let year = nu.year(); // hämtar året från objektet
let month = nu.month(); // hämtar månaden från objektet
let day = nu.day(); // hämtar dagen från objektet

println!("Idag är det {}-{}-{}", day, month, year); // skriver ut "Idag är det 8-12-2021"
```

I exemplet används `Local::now()` för att skapa ett `DateTime` objekt för nuvarande tid baserat på den lokala tidszonen. Sedan kan olika metoder användas för att få tag på specifika egenskaper hos datumen, såsom år, månad och dag.

## Djupdykning

Den aktuella datumet kan också hämtas genom att använda operativsystemets systemklocka. För att göra det, används funktionen `SystemTime::now()` från standardbiblioteket `std::time`. Detta ger en `SystemTime` objekt, som sedan kan konverteras till ett `DateTime` objekt genom att använda metoden `DateTime::from()` från `chrono`.

```Rust
use std::time::SystemTime;
use chrono::{Local, DateTime};

let now = SystemTime::now();
let now_dt: DateTime<Local> = DateTime::from(now); // konverterar till DateTime objekt

let hour = now_dt.hour(); // hämtar timmen från objektet

println!("Klockan är just nu {}.", hour); // skriver ut "Klockan är just nu 13."
```

Genom att använda `SystemTime` kan vi också göra mer avancerade operationer, såsom att jämföra datum från olika tidszoner eller skapa specifika datum för framtida händelser.

## Se även

* [Chrono dokumentation](https://docs.rs/chrono)
* [Rust's standardbibliotek dokumentation](https://doc.rust-lang.org/std/index.html)