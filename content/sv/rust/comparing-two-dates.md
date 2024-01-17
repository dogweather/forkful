---
title:                "Jämföra två datum"
html_title:           "Rust: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När man programmerar kan man ibland behöva jämföra två datum för att t.ex. se om ett datum kommer före eller efter ett annat. Detta är vanligt när man jobbar med kalendrar eller scheman och behöver sortera och filtrera datum. 

## Hur går det till:
För att jämföra två datum i Rust, kan man använda sig av ```PartialOrd``` traitet. Det här traitet låter oss jämföra värden som har en ordning, som till exempel siffror och datum. Genom att använda funktionen ```partial_cmp``` kan vi jämföra två datum och få tillbaka en ```Option<cmp::Ordering>``` som visar vilket datum som kommer före det andra. 

Exempelkod:
```
use std::cmp::Ordering;
use chrono::NaiveDate;

fn main() {
    let date1 = NaiveDate::from_ymd(2020, 06, 30);
    let date2 = NaiveDate::from_ymd(2020, 07, 01);

    match date1.partial_cmp(&date2) {
        Some(Ordering::Less) => println!("{} kommer före {}", date1, date2),
        Some(Ordering::Equal) => println!("{} är samma som {}", date1, date2),
        Some(Ordering::Greater) => println!("{} kommer efter {}", date1, date2),
        None => println!("Ingen ordning för dessa datum")
    }
}
```

Exempeloutput:
```
2020-06-30 kommer före 2020-07-01
```

## Deep Dive:
Historiskt sett har det funnits olika sätt att jämföra datum, beroende på vilket kalendersystem som används (t.ex. gregorianska eller julianska kalendern). I moderna programmeringsspråk som Rust används vanligtvis en tidsstandard som kallas Coordinated Universal Time (UTC) för att undvika komplikationer med olika kalendrar. 

Det finns också alternativa metoder för att jämföra datum, såsom att konvertera datum till en numrering som börjar vid ett visst datum och sedan jämföra dessa nummer. Det är viktigt att noga överväga vilken metod som passar bäst för ens specifika användningsområde när man jämför datum i en applikation.

När det gäller implementationen i Rust, använder ```PartialOrd``` traitet funktionen ```partial_cmp``` för att jämföra två värden. Om funktionen returnerar ```None``` betyder det att värdena inte kan jämföras, till exempel om de är i olika tidszoner eller format. 

## Se även:
- [Chrono biblioteket](https://docs.rs/chrono/latest/chrono/index.html): Ett populärt bibliotek för att hantera datum och tider i Rust.
- [Official Rust documentation](https://doc.rust-lang.org/std/cmp/trait.PartialOrd.html): Mer information om ```PartialOrd``` traitet och andra jämförelsemetoder i Rust.