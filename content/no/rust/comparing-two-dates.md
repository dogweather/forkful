---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sammenligning av to datoer betyr å bestemme hvilken av de er tidligst eller senest, eller om de er identiske. Dette er en vanlig operasjon for programmerere for å håndtere hendelser som skal skje på bestemte tidspunkter eller rekkefølger.

## Hvordan:

Her er en enkel metode for å sammenligne to datoer i Rust.

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let date1: DateTime<Utc> = Utc::now();
    let date2: DateTime<Utc> = Utc::now();

    if date1 < date2 {
        println!("date1 er tidligere enn date2");
    } else if date1 > date2 {
        println!("date1 er senere enn date2");
    } else {
        println!("date1 og date2 er identiske");
    }
}
```

Sample utgang:

``` 
date1 og date2 er identiske
```

## Dyp Dykk

1. Historisk kontekst: Tidligere språk, som C og C++, manglet innebygde mekanismer for håndtering av dato og tid. I Rust, `chrono` biblioteket gir robust funksjonalitet for dato og tid.

2. Alternativer: Vi kan også bruke Duration-funksjonaliteten til å sammenligne to datoer på en mer detaljert måte.

3. Implementasjonsdetaljer: `chrono` biblioteket tillater sammenligning av datoer ved hjelp av standard sammenligningsoperatorer, hvilket forenkler koden.

## Se Også

Ta en titt på den offisielle dokumentasjonen for `chrono` biblioteket for å lære mer om hvordan du håndterer dato og tid i Rust: [Chrono Library Documentation](https://docs.rs/chrono/0.4.19/chrono/)