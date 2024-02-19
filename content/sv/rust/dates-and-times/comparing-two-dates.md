---
aliases:
- /sv/rust/comparing-two-dates/
date: 2024-01-20 17:33:39.164901-07:00
description: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att avg\xF6ra om ett datum\
  \ kommer f\xF6re, \xE4r samma, eller efter ett annat. Programmerare g\xF6r detta\
  \ f\xF6r att hantera h\xE4ndelser,\u2026"
lastmod: 2024-02-18 23:08:51.595763
model: gpt-4-1106-preview
summary: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att avg\xF6ra om ett datum kommer\
  \ f\xF6re, \xE4r samma, eller efter ett annat. Programmerare g\xF6r detta f\xF6\
  r att hantera h\xE4ndelser,\u2026"
title: "J\xE4mf\xF6ra tv\xE5 datum"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att avgöra om ett datum kommer före, är samma, eller efter ett annat. Programmerare gör detta för att hantera händelser, tidslinjer, deadlines och tidsbaserade funktioner i applikationer.

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
