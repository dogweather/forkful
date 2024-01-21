---
title:                "Generering av tilfeldige tall"
date:                  2024-01-20T17:50:14.906484-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generering av tilfeldige tall"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall handler om å lage tall som ikke følger et forutsigbart mønster. Programmerere trenger dette for alt fra spillmekanikk til sikkerhet for å sikre uforutsigbarhet og rettferdighet.

## Hvordan:

Rust har et populært bibliotek kalt `rand` for tilfeldige tall. Installér det med Cargo og bruk det slik:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    let tilfeldig_tall: i32 = rng.gen_range(1..=10);
    println!("Tilfeldig tall mellom 1 og 10: {}", tilfeldig_tall);
}
```

Kjører du koden, kan du få ulike resultater:

```
Tilfeldig tall mellom 1 og 10: 7
```

Eller ved neste kjøring:

```
Tilfeldig tall mellom 1 og 10: 3
```

## Dypdykk:

Før `rand` biblioteket ble populært i Rust, var generering av tilfeldige tall mer komplekst og feilutsatt. `rand` er nå standarden og gir ulike metoder for tilfeldighet, inkludert kryptografisk sikre algoritmer.

Alternativer finnes for spesifikke behov; for eksempel kan `fastrand` være raskere for ikke-kryptografiske formål.

Når man genererer tilfeldige tall, bruker man ofte en "seed" for å initiere tallrekken. Uten `rand`, måtte du håndtere seeds og algoritmer selv, noe som kunne introdusere sikkerhetsrisikoer.

## Se Også:

- [Dokumentasjon for rand](https://docs.rs/rand/)
- [Rust Playground for eksperimentering](https://play.rust-lang.org/)