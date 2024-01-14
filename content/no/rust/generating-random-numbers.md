---
title:    "Rust: Generering av tilfeldige tall"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang ønsket å lage et spill eller en applikasjon der tilfeldighet spiller en viktig rolle? Da trenger du å lære hvordan du genererer tilfeldige tall i din kode! Det kan være nyttig for å skape varierende resultater i simuleringer, lage tilfeldige karakterer eller generere tilfeldige utgangspunkt i et spill.

## Hvordan generere tilfeldige tall i Rust

I Rust kan vi bruke standardbiblioteket til å generere tilfeldige tall. Vi må bare inkludere `rand` biblioteket i vår kode og bruke funksjonen `thread_rng()` for å få en tilfeldig nummergenerator. La oss se et eksempel:

```Rust
extern crate rand; // inkluderer rand biblioteket

use rand::Rng; // importerer rand::Rng i vår namespace

fn main() {
    let mut rng = rand::thread_rng(); // oppretter en ny tilfeldig nummergenerator
    let num: i32 = rng.gen(); // genererer et tilfeldig tall av typen i32
    println!("Tilfeldig tall: {}", num);
}
```

Dette vil produsere en utgang som dette: `Tilfeldig tall: 5210997089679590147`. Som du kan se, er tallet helt tilfeldig og kan variere hver gang koden kjøres.

Vi kan også spesifisere et område for de tilfeldige tallene ved å bruke `gen_range()` funksjonen. La oss si at vi bare vil ha tilfeldige tall mellom 1 og 10:

```Rust
let num: i32 = rng.gen_range(1, 11); // genererer et tilfeldig tall mellom 1 og 10
```

Vi kan også generere tilfeldige boolske verdier ved å bruke `gen()` funksjonen og spesifisere typen:

```Rust
let boolean: bool = rng.gen(); // genererer en tilfeldig boolsk verdi
```

## En dypere titt på tilfeldige tall i Rust

Hvis du er interessert i å lære mer om hvordan tilfeldige tall blir generert, kan du ta en titt på `rand` biblioteket og dens implementasjon av `thread_rng()` funksjonen. Det bruker operativsystemets kryptografisk-sikre tilfeldige tallgenerator for å sikre at tallene virkelig er tilfeldige.

En annen interessant ting å merke seg er at Rust har en `SeedableRng` trait som lar deg kontrollere startpunktet for tilfeldige tall. Dette kan være nyttig for testing eller for å kunne generere de samme tilfeldige tallene ved senere kjøring av koden.

## Se også

- Offisiell dokumentasjon for `rand` biblioteket: https://docs.rs/rand/0.8.4/rand/
- Tutorial på å generere tilfeldige tall i Rust: https://crates.io/crates/rand
- Diskusjon om sikkerhet og tilfeldighet i Rust: https://www.reddit.com/r/rust/comments/4j9qso/discussion_best_practices_for_random_number/