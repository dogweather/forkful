---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:53.119027-07:00
description: "Hvordan: Rusts standardbibliotek tilbyr en begrenset, men rask m\xE5\
  te \xE5 f\xE5 tak i gjeldende tid p\xE5, om ikke direkte den gjeldende datoen i\
  \ et kalenderformat.\u2026"
lastmod: '2024-03-13T22:44:40.584397-06:00'
model: gpt-4-0125-preview
summary: "Rusts standardbibliotek tilbyr en begrenset, men rask m\xE5te \xE5 f\xE5\
  \ tak i gjeldende tid p\xE5, om ikke direkte den gjeldende datoen i et kalenderformat."
title: "F\xE5 dagens dato"
weight: 29
---

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
