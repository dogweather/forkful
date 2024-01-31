---
title:                "Umgang mit komplexen Zahlen"
date:                  2024-01-26T04:45:10.090048-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen bestehen aus einem Realteil und einem Imaginärteil und sind in verschiedenen Bereichen wie Ingenieurwesen, Physik und Computergrafik unerlässlich. Programmierer verwenden sie, um Gleichungen zu lösen, die mit gewöhnlichen reellen Zahlen nicht lösbar sind.

## Wie geht das:
Rust bietet keine integrierte Unterstützung für komplexe Zahlen, aber Crates wie `num-complex` stehen Ihnen zur Seite. So verwenden Sie es:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let product = a * b;

    println!("Summe: {}", sum); // Summe: 3 - 1i
    println!("Produkt: {}", product); // Produkt: 14 - 5i
}
```
Sie müssen `num_complex` zu Ihrer `Cargo.toml` hinzufügen, um diese Magie zu ermöglichen.

## Tiefergehend
Komplexe Zahlen wurden im 16. Jahrhundert konzipiert, aber sie nahmen wirklich im 18. Jahrhundert Fahrt auf, als Mathematiker wie Euler begannen, mit ihnen zu spielen.

Ohne native Operationen für komplexe Zahlen verlassen sich Sprachen wie Rust auf Drittanbieter-Bibliotheken. `num-complex` ist eine solche Crate und Teil der `num` Crate-Sammlung, die darauf abzielt, numerische Typen und Traits für Rust bereitzustellen.

Es ist erwähnenswert, dass einige Sprachen (wie Python) integrierte Unterstützung für komplexe Zahlen bieten, während andere (wie C++, mit dem `<complex>` Header) sie als Teil der Standardbibliothek bereitstellen. In Rust bedeutet die Entscheidung, die Standardbibliothek klein zu halten, dass man oft zu von der Community erstellten Crates für zusätzliche Funktionalitäten greift.

## Siehe auch
- [Rust Buch](https://doc.rust-lang.org/book/): Um mehr über Rust zu erfahren und wie man mit externen Crates arbeitet.
- [Komplexe Zahlen Wikipedia](https://de.wikipedia.org/wiki/Komplexe_Zahl): Für ein tieferes Verständnis komplexer Zahlen.
