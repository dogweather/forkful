---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad och varför?

Att jämföra två datum är, precis som det låter, processen att utvärdera hur två specifika datum relaterar till varandra. Programmerare behöver denna operation för att ordna händelser, mäta tid mellan dem, eller bara för att avgöra om en tidpunkt har inträffat.


## Hur man gör:

Här är ett exempel på hur man kan jämföra två datum i Rust:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let datum1: DateTime<Utc> = Utc::now();
    let datum2: DateTime<Utc> = Utc::now();
    
    if datum1 == datum2 {
        println!("Datumen är lika");
    } else if datum1 < datum2 {
        println!("Datum1 är före datum2");
    } else {
        println!("Datum1 är efter datum2");
    }
}
```
Kör ovanstående kod och du kommer att se utmatning baserat på de nuvarande datum och tider.


## Djupdykning

Rusts `chrono` bibliotek, som vi använde i ovanstående exempel, har en rik uppsättning av datum och tid funktionalitet. Detta bibliotek skapades för att övervinna begränsningarna hos tidigare bibliotek, som `time`, genom att tillhandahålla mer fullständiga, nogranna, och user-friendly funktioner. Vad gäller alternativ, kan specificering av tidszoner och skottår vara faktorer att överväga.

Också viktigt att notera är Rusts representational conservatism kring jämförelser av två datum: Det är tyst när det gäller att hoppa över verification steps för giltighet och likhet. Detta utgör en väsentlig skillnad mot andra språk som kan förenkla jämförelser genom att ersätta externa åtgärder.

För ytterligare detaljer, utforska `DateTime`, `Date`, och `Duration` struct dokumentation. Att förstå dem bättre kommer sannolikt att förbättra kvaliteten på dina datum och tid operationer.

## Se också

För mer läsning om detta, ta en titt på följande resurser:

- [Chrono dokumentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Chrono på GitHub](https://github.com/chronotope/chrono)
- [Rust By Example](https://doc.rust-lang.org/stable/rust-by-example)