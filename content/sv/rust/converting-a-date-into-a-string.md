---
title:                "Rust: Omvandla ett datum till en sträng"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en datum till en sträng är ett vanligt problem inom programmering. Det kan vara användbart när man behöver presentera datum på ett visuellt sätt för användare eller för att skapa unika identifierare baserade på datumet. I den här bloggposten kommer jag att visa dig hur du kan utföra detta enkelt med hjälp av Rust.

## Hur man gör

Det första steget är att inkludera `chrono` biblioteket i ditt projekt. Använd sedan `DateTime` strukturen för att skapa ett datum och tid objekt.

```Rust
extern crate chrono;
use chrono::{DateTime, TimeZone, Utc};
let now = Utc::now();
```

Detta skapar ett nuvarande datum och tid objekt baserat på UTC-tidszonen. Du kan också ange en specifik tidszon genom att använda `FixedOffset` eller `Local` istället för `Utc`.

För att konvertera datumet till en sträng använder vi `format` funktionen och specificerar det önskade formatet enligt `strftime` syntax.

```Rust
let date_string = now.format("%Y-%m-%d").to_string();
```

Detta kommer att skapa en sträng som visar datumet i formatet `ÅÅÅÅ-MM-DD` (t.ex. 2021-05-24).

## Djupdykning

Om du vill ha mer kontroll över hur datumet presenteras kan du använda `NaiveDateTime` istället för `DateTime`. Detta ger dig tillgång till datum- och tidskomponenterna separat, så att du kan bygga din egen sträng baserat på dessa.

```Rust
let timestamp = now.timestamp(); // antalet sekunder sedan 1970-01-01 00:00:00 UTC
let date = now.date(); // datumkomponenten av DateTime objektet
let time = now.time(); // tidskomponenten av DateTime objektet
```

Du kan också utnyttja `DateTime` och `NaiveDateTime` metoder för att göra mer avancerade operationer, till exempel att lägga till eller subtrahera en viss tidsperiod.

## Se även

- [chrono documentation](https://docs.rs/chrono/)
- [String formatting in Rust](https://doc.rust-lang.org/std/fmt/#formatting-traits)
- [Date and time in Rust: A primer](https://blog.logrocket.com/a-primer-to-date-and-time-handling-in-rust/)