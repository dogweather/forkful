---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:50:03.639568-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att generera slumptal innebär att skapa tal som inte visar något mönster i deras uppkomst, de är oförutsägbara. Programmerare använder slumptal för allt från spelutveckling till kryptografi.

## Hur gör man:
Rust använder `rand`-cratet för slumptalsgenerering. Installera först `rand` genom att lägga till det i ditt `Cargo.toml`:

```toml
[dependencies]
rand = "0.8"
```

Sedan är det bara att börja. Här är hur du skapar ett enkelt program som genererar ett random hel- och flyttal:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Slumpmässigt heltal mellan 1 och 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Slumpmässigt heltal: {}", random_int);

    // Slumpmässigt flyttal mellan 0.0 och 1.0
    let random_float: f64 = rng.gen();
    println!("Slumpmässigt flyttal: {}", random_float);
}
```

Kör programmet flera gånger för att se olika resultat.

## Deep Dive
Historiskt sett har datorer haft problem med att skapa verkligt slumptal eftersom de är deterministiska maskiner. För att lösa detta använder programmerare algoritmer för pseudoslumptalsgeneratorer (PRNGs) som `rand`-cratet förlitar sig på.

Andra alternativ inkluderar att använda `std::rand`-modulen i Rust, men den är markerad som föråldrad till förmån för `rand`-cratet. För kryptografiskt säkra slumptal, använd `rand::rngs::OsRng`.

Slumptalsgenererarna i Rust drar nytta av perioder - tider innan de upprepar serier av tal. Även om PRNGs inte är verkligt slumpmässiga, räcker de för de flesta användningsfall om deras period är tillräckligt lång.

## Se även
- `rand` crate documentation: https://docs.rs/rand/
- Rust's bokkapitel om slumptal: https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
- Wikipedia om pseudoslumptalsgeneratorer: https://sv.wikipedia.org/wiki/Pseudoslumptalsgenerator
