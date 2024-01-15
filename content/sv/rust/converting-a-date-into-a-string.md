---
title:                "Konvertera ett datum till en sträng"
html_title:           "Rust: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att konvertera ett datum till en sträng är ett vanligt problem inom programmering, särskilt när det gäller att visa datum på ett användarvänligt sätt. Genom att lära sig hur man konverterar ett datum till en sträng i Rust kan du förbättra användarupplevelsen i din kod.

## Så här gör du
För att konvertera ett datum till en sträng i Rust, måste du först importera biblioteket "chrono". Sedan kan du använda funktionen "format" för att skapa en sträng av önskat datum och format. Nedan följer ett exempel på kod som visar hur man kan konvertera ett datum till en sträng:

```rust
use chrono::{Utc, DateTime, Datelike};

fn main() {
    // Skapar ett datumobjekt för dagens datum
    let today = Utc::today();
    
    // Skapar en sträng av det önskade formatet
    let date_string = format!("{}", today.format("%d %B, %Y"));
    
    // Skriver ut resultatet
    println!("Idag är det {}", date_string);
}

// Output:
// Idag är det 23 september, 2021
```

Funktionen "format" accepterar ett datumobjekt och ett format som argument. Det finns många olika formatalternativ att välja mellan, såsom år-månad-dag, månad-dag-år, osv. Du kan också lägga till tidsformat för att få en komplett datum- och tidssträng.

## Djupdykning
När du konverterar ett datum till en sträng använder Rusts "chrono" bibliotek ANSI C kompatibla formatteringssträngar. Det betyder att du kan använda samma formatteringssträngar i andra språk som också använder ANSI C standarden, vilket kan vara användbart om du behöver samarbeta med andra utvecklare.

För att utforska de olika formateringsalternativen kan du besöka [Chrono's dokumentation](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html). Där hittar du en lista med alla möjliga formatteringssträngar, samt exempel på hur resultaten ser ut.

## Se även
- [Rust's "chrono" bibliotek](https://crates.io/crates/chrono)
- [Chrono's dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [ANSI C formateringssträngar](https://www.gnu.org/software/libc/manual/html_node/Formatting-Calendar-Time.html#Formatting-Calendar-Time)