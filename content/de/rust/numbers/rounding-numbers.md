---
date: 2024-01-26 03:46:37.153078-07:00
description: "Wie: Rust macht das Runden zum Kinderspiel. Schauen Sie sich diese Methoden\
  \ f\xFCr die Typen `f32` oder `f64` an."
lastmod: '2024-03-13T22:44:53.667703-06:00'
model: gpt-4-0125-preview
summary: Rust macht das Runden zum Kinderspiel.
title: Zahlen runden
weight: 13
---

## Wie:
Rust macht das Runden zum Kinderspiel. Schauen Sie sich diese Methoden für die Typen `f32` oder `f64` an:

```rust
fn main() {
    let num = 2.34567;

    // Auf die nächste ganze Zahl runden
    let round = num.round();
    println!("Runden: {}", round); // Runden: 2

    // Floor - die größte ganze Zahl, die kleiner oder gleich der Zahl ist
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - die kleinste ganze Zahl, die größer oder gleich der Zahl ist
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - ganzzahliger Teil ohne Nachkommastellen
    let trunc = num.trunc();
    println!("Truncate: {}", trunc); // Truncate: 2

    // Auf das nächste Vielfache einer Zehnerpotenz runden
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Auf 2 Dezimalstellen gerundet: {}", multiple_of_ten); // Auf 2 Dezimalstellen gerundet: 2.35
}
```

## Tiefergehend
Historisch war das Runden ausschlaggebend, um unendliche Dezimalzahlen oder irrationale Zahlen in begrenzten digitalen Räumen unterzubringen – ein Muss für alte Computer mit knappem Speicher. Denken Sie an einen Abakus, aber weniger kunstvoll, mehr Mathematik.

Alternativen zu den nativen Rust-Methoden umfassen:
1. Die `format!`-Makro für die String-Formatierung, die standardmäßig rundet.
2. Externe Crates für spezialisierte mathematische Aufgaben, wie das `round`-Crate mit mehr granularer Steuerung.

Unter der Haube entsprechen Rusts Rundungsoperationen den IEEE-Standards - Fachjargon für "es rundet so, wie Ihr Mathelehrer es möchte." Außerdem können aufgrund von binären Darstellungen einige Zahlen nicht traditionell gerundet werden, wie zum Beispiel 0,1, aufgrund ihrer unendlichen Darstellung im Binärsystem.

## Siehe auch
- Rust-Dokumentation zu Methoden primitiver Typen: https://doc.rust-lang.org/std/primitive.f64.html
- IEEE-Standard für Gleitkomma-Arithmetik (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- "round" Crate für komplexeres Runden: https://crates.io/crates/round
