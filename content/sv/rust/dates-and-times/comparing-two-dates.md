---
date: 2024-01-20 17:33:39.164901-07:00
description: "Hur man g\xF6r: Datumj\xE4mf\xF6relse \xE4r en central del av rust-programmering\
  \ med `chrono`-biblioteket som standardvalet f\xF6r hantering av datum och tid.\
  \ Innan\u2026"
lastmod: '2024-04-05T22:50:52.001315-06:00'
model: gpt-4-1106-preview
summary: "Datumj\xE4mf\xF6relse \xE4r en central del av rust-programmering med `chrono`-biblioteket\
  \ som standardvalet f\xF6r hantering av datum och tid."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

## Hur man gör:
```Rust
use chrono::{DateTime, Utc, Duration};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    let later = now + Duration::days(5);

    if now < later {
        println!("Nu är före senare.");
    } else if now > later {
        println!("Nu är efter senare.");
    } else {
        println!("Det är samma tidpunkt.");
    }
}
```
Resultat:
```
Nu är före senare.
```

## Djupdykning:
Datumjämförelse är en central del av rust-programmering med `chrono`-biblioteket som standardvalet för hantering av datum och tid. Innan `chrono`, kämpade programmerare med standardtidsbiblioteket, som hade begränsningar och var mindre intuitivt. Alternativ inkluderar bibliotek som `time` och `date`. Implementationen av datumjämförelser i Rust lägger stor vikt på typsäkerhet och klarhet, där `chrono` tillhandahåller ett robust sätt att representera tidspunkter som är lätt att jämföra direkt.

## Se även:
- [Chrono Documentation](https://docs.rs/chrono/)
- [Rust by Example: Custom Types/Dates](https://doc.rust-lang.org/rust-by-example/custom_types/structs.html)
- [The Time Crate - Rust alternative to Chrono](https://docs.rs/time/)
