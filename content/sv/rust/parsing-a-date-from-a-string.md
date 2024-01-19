---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att parsa ett datum från en sträng innebär att vi omvandlar en textrepresentation av ett datum till ett datumobjekt. Programmerare gör detta för att enkelt kunna manipulera och använda datum i deras kod.

## Hur man gör:
Här är ett exempel på hur man gör det i Rust:

```Rust
use chrono::{DateTime, NaiveDateTime, Datelike, Utc, FixedOffset, offset::TimeZone};
let dt = Utc.datetime_from_str("2021-09-15 12:34:56", "%Y-%m-%d %H:%M:%S");
println!("{}", dt.unwrap());
```

När du kör det här kodstycket kommer utskriften se ut såhär: 

```Rust
2021-09-15T12:34:56Z
```

## Djupdykning
Dokumenthanteringssystem och databaser har traditionellt lagrat data som strängar. Därför har det varit nödvändigt att omvandla dessa strängrepresentationer av datum till ett format som kan tolkas av maskinen.

Det finns olika sätt att parsa ett datum. Ett alternativ är att använda "time" biblioteket som är mer lättviktig men bär på mindre funktionalitet än "chrono".

Designen av `chrono` är tänkt att vara robust. Det innebär att biblioteket försöker minimera antalet fel du kan göra genom att eliminera "ogiltiga" datum och tider.

## Se även
Chrono bibliotekets dokumentation: https://docs.rs/chrono/0.4.19/
Rust datum och tid guide: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misunderstandings.html
Rust 'time' biblioteket: https://docs.rs/time/0.3.5/time/