---
title:                "Få dagens dato"
aliases: - /no/rust/getting-the-current-date.md
date:                  2024-02-03T19:10:53.119027-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få dagens dato"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den gjeldende datoen i Rust er en vanlig oppgave for oppgaver som logging, tidsbaserte operasjoner, eller rett og slett for å vise datoen. I motsetning til noen språk som inkluderer dato- og tidsfunksjonalitet i sitt standardbibliotek, oppmuntrer Rust til bruk av et robust tredjepartsbibliotek, chrono, for omfattende manipulering av dato og tid på grunn av dets overlegne funksjonalitet og brukervennlighet.

## Hvordan:

### Bruke Rusts Standardbibliotek
Rusts standardbibliotek tilbyr en begrenset, men rask måte å få tak i gjeldende tid på, om ikke direkte den gjeldende datoen i et kalenderformat. Slik gjør du det:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Gjeldende tid: {} sekunder siden Unix Epoch.", n.as_secs()),
        Err(_) => panic!("SystemTime før Unix Epoch!"),
    }
}
```

Utdata:
```
Gjeldende tid: 1615390665 sekunder siden Unix Epoch.
```

### Bruke Chrono-biblioteket
For mer omfattende dato- og tidsfunksjonalitet, inkludert å hente den gjeldende datoen, bør du bruke `chrono`-biblioteket. Først, legg til `chrono` i din `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Deretter kan du bruke `chrono` for å få tak i gjeldende dato:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("Gjeldende dato: {}-{}-{}", now.year(), now.month(), now.day());
}
```

Utdata:
```
Gjeldende dato: 2023-4-20
```

Chrono-biblioteket gjør det enkelt å arbeide med datoer og tider, og tilbyr et bredt spekter av funksjonaliteter utover bare å hente den gjeldende datoen, inkludert parsing, formatering, og aritmetiske operasjoner på datoer og tider.
