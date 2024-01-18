---
title:                "Parsa ett datum från en sträng"
html_title:           "Rust: Parsa ett datum från en sträng"
simple_title:         "Parsa ett datum från en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att "parsing" ett datum från en sträng innebär att konvertera ett datum i textformat till ett hanterbart datumobjekt i ett programmeringsspråk. Detta är ett vanligt problem som programmerare stöter på när de behöver hantera datum och tider i sina program.

## Så här gör du:
```Rust
use std::str::FromStr;
use chrono::NaiveDate;

let date_str = "2021-08-09";
let date = NaiveDate::from_str(date_str).unwrap();
println!("{}", date); // Output: 2021-08-09
```

För detta exempel används Rusts `NaiveDate` objekt tillsammans med `FromStr` trait för att konvertera en sträng till ett datumobjekt. Detta inkluderar inte tid eller tidszon information.

## Djupdykning:
Parsing av datum från en sträng har varit ett problem inom programmering sedan länge. Tidigare användes bibliotek som `time.h` i C för att hantera datum, men med tiden har bättre lösningar utvecklats, som Chrono biblioteket i Rust. Det finns också alternativ som `serde`, som fokuserar på att serialisera och deserialisera data, vilket också inkluderar parsing av datum från strängar.

När det gäller implementationen så kan `FromStr` trait användes för att konvertera en sträng till ett datumobjekt, men det finns också andra metoder, som att använda ett reguljärt uttryck för att matcha och extrahera datum från en sträng.

## Se även:
- [Chrono biblioteket i Rust](https://docs.rs/chrono/latest/chrono/index.html)
- [Serde biblioteket i Rust](https://serde.rs/)