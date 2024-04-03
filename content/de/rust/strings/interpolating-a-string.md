---
date: 2024-01-20 17:51:35.946663-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:53.658831-06:00'
model: gpt-4-1106-preview
summary: .
title: Zeichenketten interpolieren
weight: 8
---

## How to:
```Rust
fn main() {
    let planet = "Erde";
    let population = 7_753_000_000;

    // Direkte Interpolation mit format! Makro
    let message = format!("Willkommen auf der {}, Bevölkerung: {}.", planet, population);
    println!("{}", message);

    // Interpolation mit Platzhaltern
    println!("{} hat etwa {} Einwohner.", planet, population);
}
```
Ausgabe:
```
Willkommen auf der Erde, Bevölkerung: 7753000000.
Erde hat etwa 7753000000 Einwohner.
```

## Deep Dive
Die String-Interpolation in Rust wird hauptsächlich durch das `format!` Makro ermöglicht, ähnlich zu Sprachen wie Python oder Ruby. Rust benutzt jedoch keine eingebauten String-Methoden dafür. Historisch gesehen war Rust strenger, um zur Kompilierzeit sicherzustellen, dass die Typen übereinstimmen und die Ausführung sicher ist.

Alternativen zur Interpolation sind String-Konkatenation mit dem `+` Operator oder dem `format!` ähnliche Makros wie `write!` oder `writeln!`, für Ausgaben direkt in `io::Write`-Traits.

In Rust erfolgt die Interpolation nicht direkt im String, da es keinen speziellen Syntax wie in anderen Sprachen gibt. Es ist eine sichere und kontrollierte Form, die Rust's Fokus auf Sicherheit und Performanz reflektiert.

## See Also
- Rust's format! syntax: https://doc.rust-lang.org/stable/std/fmt/
- Rust by Example – Formatted print: https://doc.rust-lang.org/rust-by-example/hello/print.html
- The Rust Programming Language – String Type: https://doc.rust-lang.org/book/ch08-02-strings.html
