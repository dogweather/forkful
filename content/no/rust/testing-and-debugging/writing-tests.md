---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:04.838521-07:00
description: "\xC5 skrive tester i Rust inneb\xE6rer \xE5 lage automatiserte sjekker\
  \ for \xE5 sikre at koden din fungerer som forventet. Programmerere gj\xF8r dette\
  \ for \xE5 fange opp\u2026"
lastmod: '2024-03-13T22:44:40.577395-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive tester i Rust inneb\xE6rer \xE5 lage automatiserte sjekker for\
  \ \xE5 sikre at koden din fungerer som forventet. Programmerere gj\xF8r dette for\
  \ \xE5 fange opp\u2026"
title: Skrive tester
weight: 36
---

## Hva & Hvorfor?

Å skrive tester i Rust innebærer å lage automatiserte sjekker for å sikre at koden din fungerer som forventet. Programmerere gjør dette for å fange opp feil tidlig, lette omstrukturering, og opprettholde kodens kvalitet over tid.

## Hvordan:

Rusts innebygde testrammeverk støtter enhets-, integrasjons- og dokumentasjonstester uten behov for eksterne biblioteker. Tester er annotert med `#[test]`, og enhver funksjon annotert slik blir kompilert som en test.

### Skrive en EnhetsTest:

Plasser enhetstester i modulen de tester ved å bruke en `tests` undermodul merket med `#[cfg(test)]` for å sikre at de bare kompileres når det testes.

```rust
// lib.rs eller main.rs
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_adds_two() {
        assert_eq!(add(2, 2), 4);
    }
}
```

Kjøre tester:
```shell
$ cargo test
```

Utdata:
```shell
   Compiling your_package_name v0.1.0 (/path/to/your_package)
    Finished test [unoptimized + debuginfo] target(s) in 0.00 secs
     Running unittests src/lib.rs (or src/main.rs)

running 1 test
test tests::it_adds_two ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### Skrive Integrasjonstester:

Integrasjonstester plasseres i en tests mappe på toppnivået av prosjektet ditt, ved siden av `src`. Hver `.rs`-fil i `tests` kompileres som sin egen separate crate.

```rust
// tests/integration_test.rs
use your_package_name;

#[test]
fn it_adds_two() {
    assert_eq!(your_package_name::add(2, 2), 4);
}
```

### Testing med Populære Tredjepartsbiblioteker:

For mer omfattende testingsegenskaper, kan biblioteket `proptest` generere et bredt spekter av inndata for å teste funksjoner.

Legg til `proptest` som en dev avhengighet i `Cargo.toml`:

```toml
[dev-dependencies]
proptest = "1.0"
```

Bruk `proptest` for å kjøre samme test med mange automatisk genererte inndata:

```rust
// inne i tests/integration_test.rs eller en moduls #[cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn doesnt_crash(a: i32, b:i32) {
        your_package_name::add(a, b);
    }
}
```

Dette sjekker at `add` ikke panikker for et bredt spekter av `i32` inndata.
