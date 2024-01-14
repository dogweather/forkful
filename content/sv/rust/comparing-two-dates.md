---
title:    "Rust: Jämförelse av två datum"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
I denna bloggpost kommer vi att utforska hur man kan jämföra två datum i Rust-programmering. Detta kan vara användbart för att hantera och sortera data som innehåller datum, till exempel i ett kalenderprogram eller en faktureringsapplikation.

## Så här gör du
För att börja jämföra två datum i Rust, behöver vi först importera standardbiblioteket `chrono`. Sedan kan vi skapa två variabler med olika datum och sedan jämföra dem med hjälp av funktionen `>`, `<` eller `==`.

```Rust
use chrono::{Datelike, NaiveDate};
// Skapar två datum
let start_date = NaiveDate::from_ymd(2020, 5, 10); // 10 maj 2020
let end_date = NaiveDate::from_ymd(2020, 5, 12); // 12 maj 2020
if start_date < end_date {
    println!("Startdatumet är tidigare än slutdatumet.");
}
```

Output:
```
Startdatumet är tidigare än slutdatumet.
```

Vi kan också använda funktionen `Ord` för att jämföra datum med `>, <, <=, >=` operatorer.

```Rust
use chrono::{Datelike, NaiveDate};
// Skapar två datum
let start_date = NaiveDate::from_ymd(2020, 5, 10); // 10 maj 2020
let end_date = NaiveDate::from_ymd(2020, 5, 12); // 12 maj 2020
if start_date.cmp(&end_date) == Ordering::Less {
    println!("Startdatumet är tidigare än slutdatumet.");
}
```

Output:
```
Startdatumet är tidigare än slutdatumet.
```


## Djupdykning
För att förstå hur jämförelsen av datum fungerar i Rust, är det viktigt att förstå hur `chrono` biblioteket hanterar datum och tider. I `chrono` representeras datum av strukturen `NaiveDate`, som innehåller årtal, månad och dag. När vi jämför två datum använder Rust de inbyggda operatorerna för strängar, som jämför varje del av datumet.

Det är också viktigt att notera att `chrono` biblioteket följer gregorianska kalendern, och det finns funktioner för att hantera olika tidzoner och kalendrar.

## Se även
- [Dokumentation för chrono biblioteket](https://docs.rs/chrono/0.4.19/chrono/)
- [Enkel jämförelse av datum i Rust](https://rust-lang-nursery.github.io/rust-cookbook/datetime/compare.html)