---
title:                "Finn lengden på en streng"
date:                  2024-01-20T17:48:09.542464-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng i Rust betyr å telle antall tegn den inneholder. Programmerere trenger denne informasjonen for å validere inndata, manipulere tekst, eller bare for å sjekke størrelsen på en streng.

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
