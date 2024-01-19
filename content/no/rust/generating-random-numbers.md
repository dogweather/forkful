---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Generere tilfeldige tall i Rust: En enkel guide

Lær hvordan du kan generere tilfeldige tall i Rust programmeringsspråk.

## Hva & Hvorfor?

Generering av tilfeldige tall er prosessen for å lage tall som ikke kan forutsies bedre enn ved en tilfeldig sjanse. Programmerere gjør dette hovedsakelig for å simulere tilfeldige situasjoner i spill, dataanalyse og testing av programvare.

## Hvordan gjør man det:

Her stiller vi et enkelt eksempel på hvordan du kan generere et tilfeldig tall mellom 1 og 10 i Rust:

```Rust
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    let n: u8 = rng.gen_range(1..11);
    println!("{}", n);
}
```

Når du kjører programmet over, vil det skrive ut et tilfeldig tall mellom 1 og 10.

## Dypdykk

Historisk sett brukte de tidligste datamaskinene fysiske prosesser til å generere tilfeldige tall. I dagens høytytende og høy-trådet datamiljøer bruker programmeringsspråk som Rust pseudorandom number generators (PRNGs). Dette er algoritmer som bruker matematiske prosedyrer for å produsere tilsynelatende tilfeldige resultater. 

Som et alternativ kan du også vurdere bruk av eksterne biblioteker som pcg_rand og fastrand.

Når det gjelder implementeringsdetaljer i Rust, bruker det rand::Rng-trait og funksjonen gen_range for å generere tilfeldige tall. rand::Rng er en trait som definerer metoder som genererer tilfeldige tall og trait er tilgjengelig ved å importere rand crate.

## Se også

Du kan lese mer om generering av tilfeldige tall i Rust fra følgende kilder:

1. Rust sin offisielle dokumentasjon på [rand crate](https://docs.rs/rand)
2. Diskusjon om [tilfeldige tall generasjon på Rust sin brukerforum](https://users.rust-lang.org/t/random-number-generation/17225)
3. Artikkel om [Rust Random Number Generation på StackOverflow](https://stackoverflow.com/questions/44377020/rust-random-number-generation)

Husk, det beste er å eksperimentere på egen hånd. Lykke til med programmeringen din!