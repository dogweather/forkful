---
title:    "Rust: Jämföring av två datum"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Varför jämföra två datum i Rust?

När man arbetar med datum och tider är det ofta nödvändigt att jämföra dem för att kontrollera vilket som är tidigare eller senare. Detta är särskilt viktigt inom programmering, där korrekta jämförelser är avgörande för att undvika buggar och felaktiga resultat. I denna bloggpost kommer vi att gå igenom hur man kan jämföra två datum i Rust och vilken betydelse det har för dina program.

## Så här gör du

För att kunna jämföra två datum i Rust behöver vi först skapa två variabler som representerar dessa datum. Vi kan använda oss av standardbibliotekets modul `std::time::SystemTime` för att skapa ett datum med tiden då vårt program körs:

```Rust
use std::time::SystemTime;

let now = SystemTime::now();
```

För att skapa ett specifikt datum kan vi använda en annan modul i standardbiblioteket, nämligen `std::time::Duration`. I exemplet nedan skapar vi ett datum som är 7 dagar efter dagens datum:

```Rust
use std::time::Duration;

let seven_days_from_now = now + Duration::from_secs(7 * 24 * 60 * 60);
```

Nu har vi två variabler som representerar två olika datum, och vi kan jämföra dem genom att använda operatorn `<` eller `>` beroende på vad vi är intresserade av. Till exempel kan vi undersöka om `seven_days_from_now` är senare än `now`:

```Rust
if seven_days_from_now > now {
    println!("seven_days_from_now är senare än now");
}
```

Detta uttryck ger oss en sanningsvärde som vi sedan kan använda för att fatta beslut eller utföra andra åtgärder i vårt program.

## Djupdykning

När vi jämför två datum på det här sättet används en datatyp som heter `std::time::Duration`. Denna typ representerar en tidsperiod och kan vara positiv (till exempel 5 minuter) eller negativ (till exempel -3 sekunder). När vi adderar en `Duration` till ett datum, som i exemplet ovan, räknas tidpunkten ut efter antalet sekunder som vi har specificerat. Detta innebär att vi kan hantera både positiva och negativa tidsperioder på ett enkelt sätt.

För mer avancerade jämförelser finns det också en modul `std::time::Instant` som representerar en specifik tidsstämpel i sekunder och nanosekunder. Denna typ är användbar för att mäta exakta tidsintervaller och är särskilt användbar för prestandaanalyser.

## Se även

- [Rust Standard Library Documentation](https://doc.rust-lang.org/std/index.html)
- [Rust Date and Time Library](https://github.com/chronotope/chrono)
- [Date and Time Functions in Rust](https://www.tutorialspoint.com/rust/rust_date_time.htm)