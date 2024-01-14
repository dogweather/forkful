---
title:                "Rust: Jämföring av två datum"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

I vardagen sätter vi ofta in och jämför olika datum, från att planera möten till att lägga in deadlines i vår kalender. Men ibland kan det vara en utmaning att jämföra datum i våra program, speciellt om vi använder olika format eller om vi behöver hantera olika tidszoner. Det är här som Rust kommer in i bilden och erbjuder en robust lösning för att jämföra datum på ett enkelt och effektivt sätt.

## Så här gör du

För att jämföra två datum i Rust kan du använda "Ord" traiten, som finns tillgänglig för alla standarddatumstyper. Detta gör det möjligt att jämföra två datum med hjälp av jämförelseoperatorer som "<" eller "==". Låt oss ta en titt på ett exempel:

```Rust
use std::cmp::Ordering;
use std::time::SystemTime;

fn main() {
    let date1 = SystemTime::now();
    let date2 = date1 + Duration::from_secs(3600); // date2 kommer att vara ett datum som ligger en timme efter date1
    
    match date1.cmp(&date2) {
        Ordering::Less => println!("date1 är tidigare än date2"),
        Ordering::Greater => println!("date1 är senare än date2"),
        Ordering::Equal => println!("date1 är samma som date2"),
    }
}
```

I detta exempel skapar vi två datum genom att använda SystemTime-typen. Vi använder sedan "cmp" funktionen tillsammans med "Ord" traiten för att jämföra de två datumen och utskriva resultatet baserat på ordningen.

## Djupdykning

När vi jämför två datum är det viktigt att förstå att jämförelsen görs baserat på vanlig tid eller "UTC" (Coordinated Universal Time). Om du behöver jämföra datum med olika tidszoner kan du använda "OffsetDateTime" typen från "chrono" biblioteket för att inkludera tidszoninformation i dina datum.

Utöver detta kan du också använda olika funktioner för att manipulera datum, som "add" för att lägga till en viss tidsperiod till ett datum eller "subtract" för att dra ifrån en tidsperiod från ett datum.

## Se även

* [Rust dokumentation för Ord traiten](https://doc.rust-lang.org/std/cmp/trait.Ord.html)
* [Chrono biblioteket för hantering av datum i Rust](https://docs.rs/chrono/0.4.19/chrono/)
* [Jämförelse av datum på ett effektivt sätt med Rust](https://www.lpalmieri.com/posts/2021-01-23-rust-datetime-comparison/)