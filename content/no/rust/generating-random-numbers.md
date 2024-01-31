---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:35:12.817430-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i Rust innebærer å bruke biblioteker for å produsere uforutsette numeriske verdier, noe som er uunnværlig for oppgaver som spenner fra kryptografi og simuleringer til spill og tilfeldige algoritmer.

## Hvordan:

Rust er avhengig av eksterne crates for generering av tilfeldige tall, der `rand` er det mest brukte. For å begynne å generere tilfeldige tall, må du først legge til `rand` i din `Cargo.toml`-fil:

```toml
[dependencies]
rand = "0.8.5"
```

Deretter kan du generere tilfeldige tall ved å bruke `rand` i din Rust-kode. Her er et eksempel på generering av et tilfeldig heltall og et flyttall:

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // Generer et tilfeldig heltall mellom 1 og 10
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Tilfeldig heltall: {}", random_int);
    
    // Generer et tilfeldig flyttall mellom 0.0 og 1.0
    let random_float: f64 = rng.gen::<f64>();
    println!("Tilfeldig flyttall: {}", random_float);
}
```

Eksempel på utdata kan være:

```plaintext
Tilfeldig heltall: 7
Tilfeldig flyttall: 0.9401077112175732
```

Merk at å kjøre programmet på nytt vil produsere forskjellige verdier.

## Dyp Dykk

Generering av tilfeldige tall i Rust, tilrettelagt gjennom `rand` og dets avhengigheter som `getrandom`, representerer en bred abstraksjon over operativsystemets fasiliteter og algoritmiske generatorer. Historisk sett har tilfeldighet i databehandling utviklet seg fra enkle, forutsigbare algoritmer til komplekse, kryptografisk sikre metoder. Rusts tilnærming inkapsulerer denne utviklingen gjennom sitt pluggbare `Rng` trait, som kan understøttes av ulike generatorer avhengig av kvaliteten på tilfeldigheten og ytelsen som kreves.

For de fleste applikasjoner gir å stole på `rand` og systemets RNG en god balanse mellom enkelhet og entropi. Imidlertid, for kryptografiske applikasjoner, vender crates som `rand` seg til `getrandom` for seeding, som igjen er avhengig av OS-spesifikke mekanismer (for eksempel `/dev/urandom` på Unix-lignende systemer), noe som sikrer kryptografisk sikker tilfeldighet.

Alternativt, hvis du har spesifikke behov som ikke møtes av `rand`, kan utforsking av andre crates eller implementering av egendefinerte generatorer basert på matematiske modeller være en vei å gå. Likevel, for det store flertallet av bruksområder, gir `rand` og dets økosystem robuste løsninger som er både effektive og enkle å integrere i Rust-applikasjoner.
