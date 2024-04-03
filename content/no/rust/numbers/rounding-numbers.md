---
date: 2024-01-26 03:46:56.343049-07:00
description: "\xC5 runde av tall betyr \xE5 justere dem til n\xE6rmeste hele tall\
  \ eller en br\xF8kdel med en viss presisjon. Programmerere runder av tall for \xE5\
  \ forenkle verdier for\u2026"
lastmod: '2024-03-13T22:44:40.569050-06:00'
model: gpt-4-0125-preview
summary: "\xC5 runde av tall betyr \xE5 justere dem til n\xE6rmeste hele tall eller\
  \ en br\xF8kdel med en viss presisjon."
title: Avrunding av tall
weight: 13
---

## Hvordan:
Rust gjør avrunding til en lek. Sjekk ut disse metodene for `f32` eller `f64` typer:

```rust
fn main() {
    let num = 2.34567;

    // Avrund til nærmeste hele tall
    let round = num.round();
    println!("Avrundet: {}", round); // Avrundet: 2

    // Gulv - største heltall mindre enn eller lik tallet
    let floor = num.floor();
    println!("Gulv: {}", floor); // Gulv: 2

    // Tak - minste heltall større enn eller lik tallet
    let ceil = num.ceil();
    println!("Tak: {}", ceil); // Tak: 3

    // Trunker - heltallsdelen uten fraksjonssifre
    let trunc = num.trunc();
    println!("Trunkert: {}", trunc); // Trunkert: 2

    // Til nærmeste multiplum av en tierpotens
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Avrundet til 2 desimaler: {}", multiple_of_ten); // Avrundet til 2 desimaler: 2.35
}
```

## Dykking Dypere
Historisk har avrunding vært avgjørende for å passe uendelige desimaler eller irrasjonelle tall i begrensede digitale rom - et must for gamle datamaskiner med knapp hukommelse. Tenk kuleramme, men mindre håndverksmessig, mer matte.

Alternativer til de innfødte Rust-metodene inkluderer:
1. `format!` makro for tekstformatering som avrunder som standard.
2. Eksterne pakker for spesialiserte matteoppgaver, som `round` pakken med mer granulær kontroll.

Under hetten etterkommer Rusts avrundingsoperasjoner IEEE-standarder - teknisk sjargong for "det avrunder slik matematikklæreren din ønsker." Pluss, på grunn av binære representasjoner, kan noen tall ikke avrundes tradisjonelt, som 0.1, på grunn av deres uendelige representasjon i binært.

## Se også
- Rust dok på primitive typemetoder: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE-standard for flyttallsaritmetikk (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round" pakke for mer kompleks avrunding: https://crates.io/crates/round
