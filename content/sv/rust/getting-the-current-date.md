---
title:                "Rust: Att hämta aktuellt datum"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda dagens datum är en viktig del av många programmeringsprojekt. Det kan hjälpa dig att spåra tidsbaserade händelser, beräkna åldrar eller helt enkelt visa dagens datum för användaren. I denna bloggpost kommer jag att visa dig hur du kan göra det med hjälp av Rust.

## Så här gör du

För att hämta dagens datum i Rust behöver vi använda ett bibliotek som heter `chrono`. Detta bibliotek tillhandahåller ett antal funktioner för att arbeta med datum och tid i Rust. För att börja använda `chrono` behöver vi först lägga till det i vår `Cargo.toml` fil:

```
[dependencies]
chrono = "0.4.19"
```

Efter att ha uppdaterat vår `Cargo.toml` fil kan vi importera `chrono` till vårt projekt:

```
use chrono::prelude::*;
```

Nu är vi redo att använda `chrono`. Vi kan hämta dagens datum genom att skapa ett `Date` objekt och sedan använda funktionen `today()`:

```
let today = Local::today();
println!("Dagens datum är: {}", today);
```

Vi kan också ange ett specifikt datum genom att använda funktionen `ymd()` som tar tre argument - år, månad och dag:

```
let specific_date = Local.ymd(2021, 10, 31);
```

Observera att `Local` är en del av `chrono` som representerar det lokala lagsystemet för datum och tid.

## Djupdykning

Nu när vi vet hur man hämtar dagens datum, låt oss titta på några andra funktioner som `chrono` erbjuder. Vi kan till exempel ta reda på vilken veckodag det är genom att använda `.weekday()` funktionen:

```
let weekday = today.weekday();
println!("Det är {} idag!", weekday);
```

Vi kan också formatera datumet på olika sätt genom att använda `.format()` funktionen. Till exempel kan vi formatera det som "dag/månad/år" genom att använda `"%e/%m/%Y"`:

```
let formatted_date = today.format("%e/%m/%Y");
println!("Datumet i dagens format är: {}", formatted_date);
```

Detta är bara några exempel på vad som är möjligt med `chrono`. Jag rekommenderar att du utforskar dokumentationen för att lära dig mer om hur du kan arbeta med datum och tid i Rust.

## Se även

- [Chrono dokumentation](https://docs.rs/chrono)
- [Rustlings: Chrono](https://github.com/rust-lang/rustlings/blob/main/exercises/chrono/chrono.rs)