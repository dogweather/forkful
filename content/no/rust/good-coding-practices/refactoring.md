---
title:                "Refaktorering"
aliases: - /no/rust/refactoring.md
date:                  2024-01-26T03:36:56.310010-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Refaktorering er prosessen med å restrukturere eksisterende dataprogramkode—endre faktoriseringen—uten å endre dens eksterne oppførsel. Programmerere gjør dette for å forbedre ikke-funksjonelle attributter av programvaren, som lesbarhet, redusert kompleksitet, forbedre vedlikeholdbarhet, og skape en mer uttrykkende intern arkitektur eller objektmodell for å forbedre utvidbarhet.

## Hvordan:

La oss refaktorere et enkelt stykke Rust-kode for å gjøre det mer idiomatisk og vedlikeholdbart. Vi starter med en funksjon som kalkulerer summen av en vektor med heltall:

```rust
fn sum(vec: &Vec<i32>) -> i32 {
    let mut sum = 0;
    for i in vec {
        sum += i;
    }
    sum
}

fn main() {
    let tall = vec![1, 2, 3, 4, 5];
    println!("Summen er {}", sum(&tall));
}
```

Output:
```
Summen er 15
```

Nå, la oss refaktorere dette for å bruke mer idiomatisk Rust ved å utnytte iteratorer og `fold`-metoden:

```rust
fn sum(vec: &[i32]) -> i32 {
    vec.iter().fold(0, |acc, &x| acc + x)
}

fn main() {
    let tall = vec![1, 2, 3, 4, 5];
    println!("Summen er {}", sum(&tall));
}
```

Ingen endring i output—den er fortsatt `15`—men den refaktorerte versjonen er renere og bruker Rusts styrker som låning og iterator metoder.

## Dypdykk

Refaktorering har sine røtter i Smalltalk-samfunnet og ble popularisert i Java-verdenen av Martin Fowlers bok "Refactoring: Improving the Design of Existing Code". Prinsippene er universelle og gjelder også for Rust, hvor sikkerhet og samtidighet er av største viktighet. Rust oppmuntrer til å skrive robust kode ved å fange problemer ved kompileringstid, så under refaktorering, fungerer Rust-kompilatoren som et sikkerhetsnett.

Alternativer til manuell refaktorering inkluderer bruk av automatiserte verktøy, som 'rustfmt' for kodeformatering og 'clippy' for linting, som kan foreslå mer idiomatiske måter å skrive kode på. Imidlertid krever dyp refaktorering ofte en gjennomtenkt forståelse av kodens design, noe disse verktøyene ikke kan automatisere fullt ut.

I Rust kan refaktorering dreie seg om å forbedre typen bruk, utnytte levetider effektivt, redusere unødvendige tildelinger, eller benytte seg av samtidighetsmønstre som å bruke `Arc<Mutex<T>>` når nødvendig. Det er også vanlig å gå over fra `unwrap()` til mer uttrykksfull feilhåndtering med `Result<T, E>`.

## Se Også

For å dykke dypere inn i refaktorering i Rust:

- The Rust Book: https://doc.rust-lang.org/book/
- Rust by Example: https://doc.rust-lang.org/rust-by-example/
- Clippy, et Rust linting-verktøy: https://github.com/rust-lang/rust-clippy
- "Refactoring: Improving the Design of Existing Code" av Martin Fowler: https://martinfowler.com/books/refactoring.html
