---
date: 2024-01-20 17:48:09.542464-07:00
description: "Hvordan gj\xF8re det: ."
lastmod: '2024-03-13T22:44:40.565002-06:00'
model: gpt-4-1106-preview
summary: .
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## Hvordan gjøre det:
```rust
fn main() {
    let tekst = "Hei, Norge!";
    let lengde = tekst.chars().count();
    
    println!("Lengden av '{}' er {}", tekst, lengde);
}
```
Output:
```
Lengden av 'Hei, Norge!' er 11
```

## Dypdykk
Helt fra Rusts tidlige dager var det å finne lengden på en streng ikke alltid så rett fram. Rusts `String` type er kodet i UTF-8, noe som gjør `.len()` metoden mindre intuitiv; den returnerer antall bytes, ikke antall tegn. Derfor bruker vi `.chars().count()` for å få det faktiske antall tegn. Et alternativ er `.bytes().count()` om man heller vil ha antall bytes, eller `.graphemes(true).count()` (fra `unicode-segmentation` craten) for å få antall grafemer, som kan være nyttig ved mer kompleks tekstbehandling.

## Se også
- Rust dokumentasjonen for strenger: https://doc.rust-lang.org/std/string/
- `unicode-segmentation` crate for å håndtere grafemer: https://crates.io/crates/unicode-segmentation
