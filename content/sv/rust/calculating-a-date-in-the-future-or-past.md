---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Rust: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att räkna ut ett datum i framtiden eller förflutet är en vanlig uppgift för programmerare. Det kan vara användbart för att planera scheman, beräkna åldrar eller helt enkelt för att få en överblick över tidsramar. 

## Så här gör du:
Låt oss säga att vi vill beräkna datumet 30 dagar framåt från idag. I Rust kan vi göra detta med hjälp av funktionen ```chrono::Utc::today()``` och ```chrono::Duration::days()```. Vi behöver först importera biblioteket chrono och sedan definiera en variabel med dagarnas räkna:

```Rust
use chrono::{Utc, Duration};
let days = Duration::days(30);
```
Sedan kan vi använda funktionen ```datedate()``` för att få dagens datum och lägga till antalet dagar som valts: 

```Rust
let future_date = Utc::today() + days;
``` 
Vi kan också göra samma sak för att beräkna ett datum i förflutet genom att använda en negativt antal dagar.

## Djupdykning:
Att beräkna datum i ett datorsystem kan vara en knepig uppgift. Det kräver noggrann hantering av tidszoner, sommar- och vintertid, och andra detaljer. Lyckligtvis gör biblioteket chrono det enkelt för oss att hantera dessa komplexiteter och räkna ut korrekta datum.

Det finns även andra sätt att räkna ut datum i Rust, till exempel genom att använda biblioteket ```time```. Det är viktigt att välja det bibliotek som passar bäst för ditt specifika projekt och behov.

## Se även:
Här är några användbara länkar för att utforska mer om hur man beräknar datum i Rust:

- Den officiella dokumentationen för biblioteket [chrono](https://docs.rs/chrono).
- En tutorial om hur man beräknar datum i Rust med hjälp av olika bibliotek [här](https://betterprogramming.pub/dates-and-times-in-rust-eb8661377eb8).