---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:29.273214-07:00
description: "Complexe getallen hebben een re\xEBel deel en een imaginair deel en\
  \ zijn cruciaal in diverse velden zoals engineering, fysica en computergrafiek.\u2026"
lastmod: '2024-03-13T22:44:50.588002-06:00'
model: gpt-4-0125-preview
summary: "Complexe getallen hebben een re\xEBel deel en een imaginair deel en zijn\
  \ cruciaal in diverse velden zoals engineering, fysica en computergrafiek."
title: Werken met complexe getallen
weight: 14
---

## Wat & Waarom?
Complexe getallen hebben een reëel deel en een imaginair deel en zijn cruciaal in diverse velden zoals engineering, fysica en computergrafiek. Programmeurs gebruiken ze om vergelijkingen op te lossen die gewone reële getallen niet aankunnen.

## Hoe te:
Rust heeft geen ingebouwde ondersteuning voor complexe getallen, maar crates zoals `num-complex` staan voor je klaar. Hier is hoe je het gebruikt:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let som = a + b;
    let product = a * b;

    println!("Som: {}", som); // Som: 3 - 1i
    println!("Product: {}", product); // Product: 14 - 5i
}
```
Je moet `num_complex` toevoegen aan je `Cargo.toml` om deze magie te laten gebeuren.

## Diepe Duik
Complexe getallen werden bedacht in de 16e eeuw maar kregen echt een vlucht in de 18e eeuw toen wiskundigen zoals Euler ermee begonnen te spelen.

Zonder native operaties voor complexe getallen, vertrouwen talen zoals Rust op bibliotheken van derden. `num-complex` is zo'n crate en maakt deel uit van de `num` crate-collectie die tot doel heeft numerieke types en traits voor Rust te bieden.

Het is vermeldenswaard dat sommige talen (zoals Python) ingebouwde ondersteuning hebben voor complexe getallen, terwijl anderen (zoals C++, met de `<complex>` header) ze als onderdeel van de standaardbibliotheek bieden. In Rust betekent de beslissing om de standaardbibliotheek klein te houden dat je vaak zult reiken naar door de gemeenschap gecreëerde crates voor extra functionaliteit.

## Zie Ook
- [Rust Book](https://doc.rust-lang.org/book/): Om meer te leren over Rust en hoe je externe crates gebruikt.
- [Complex Number Wikipedia](https://en.wikipedia.org/wiki/Complex_number): Voor een dieper begrip van complexe getallen.
