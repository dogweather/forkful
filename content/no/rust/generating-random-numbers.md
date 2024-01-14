---
title:                "Rust: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger i et programmeringsprosjekt trenger du å generere tilfeldige tall. Dette kan for eksempel være for å lage en simulator, lage spillmekanikker eller teste forskjellige algoritmer. I dette blogginnlegget skal vi se på hvordan man kan generere tilfeldige tall i Rust.

## Hvordan
Først må vi legge til "rand" biblioteket i prosjektet vårt ved å legge til følgende linje i "Cargo.toml" filen:

```Rust
[dependencies]
rand = "0.8.3" 
```

Vi importerer så "rand" biblioteket i koden vår:

```Rust
use rand::{Rng, thread_rng}; 
```

Nå kan vi bruke funksjonen "gen_range" fra "thread_rng" biblioteket for å generere et tilfeldig tall innenfor et bestemt område:

```Rust
let random_number = thread_rng().gen_range(1..11); //genererer et tall mellom 1 og 10
println!("Det tilfeldige tallet er: {}", random_number); //output: "Det tilfeldige tallet er: 5" (kan variere)
```

Vi kan også generere et tilfeldig heltall innenfor et bestemt område ved hjelp av "gen_range" funksjonen:

```Rust
let random_integer = thread_rng().gen_range(1..11); //genererer et heltall mellom 1 og 10
println!("Det tilfeldige heltallet er: {}", random_integer); //output: "Det tilfeldige heltallet er: 8" (kan variere)
```

Om du ønsker å generere et tilfeldig desimaltall, kan du bruke "gen_range" funksjonen og "uniform" metoden:

```Rust
let random_float = thread_rng().gen_range(0.0..1.0).uniform(); //genererer et desimaltall mellom 0 og 1
println!("Det tilfeldige desimaltallet er: {}", random_float); //output: "Det tilfeldige desimaltallet er: 0.4531908443" (kan variere)
```

## Deep Dive
Bak kulissene bruker "thread_rng" funksjonen en PRNG (Pseudo Random Number Generator) for å generere tilfeldige tall. Dette betyr at tallene ikke er helt tilfeldige, men følger en matematisk algoritme for å oppnå tilfeldighet. PRNG kan også gjentas ved å bruke den samme startverdien, som er grunnen til at "thread_rng" funksjonen holder styr på en intern tilstand for å sikre unike tall.

Om du ønsker mer avansert tilfeldighet, kan du bruke "EntropyRng" funksjonen fra "entropy_rng" biblioteket. Denne funksjonen genererer tilfeldige tall ved å bruke tilfeldige data fra datamaskinens operativsystem, slik at tallene er mer uforutsigbare og tilnærmet tilfeldige.

## Se Også
- [Rust dokumentasjon: Random library](https://doc.rust-lang.org/rand/index.html)
- [GitHub: Rand library repository](https://github.com/rust-random/rand)
- [Medium: Generating random numbers in Rust](https://medium.com/@stephengnl/generating-random-numbers-in-rust-97e667a1cae8)