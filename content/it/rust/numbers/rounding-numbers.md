---
title:                "Arrotondamento dei numeri"
aliases:
- /it/rust/rounding-numbers.md
date:                  2024-01-26T03:47:11.596153-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/rust/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Arrotondare i numeri significa regolarli al numero intero più vicino o a una frazione con una certa precisione. I programmatori arrotondano i numeri per semplificare i valori per la leggibilità umana, per soddisfare i requisiti delle specifiche o per ridurre il sovraccarico computazionale nelle operazioni in virgola mobile.

## Come fare:
Rust rende l'arrotondamento un gioco da ragazzi. Dai un'occhiata a questi metodi per i tipi `f32` o `f64`:

```rust
fn main() {
    let num = 2.34567;

    // Arrotonda al numero intero più vicino
    let round = num.round();
    println!("Arrotondato: {}", round); // Arrotondato: 2

    // Floor - il più grande intero minore o uguale al numero
    let floor = num.floor();
    println!("Floor: {}", floor); // Floor: 2

    // Ceil - il più piccolo intero maggiore o uguale al numero
    let ceil = num.ceil();
    println!("Ceil: {}", ceil); // Ceil: 3

    // Truncate - parte intera senza cifre frazionarie
    let trunc = num.trunc();
    println!("Troncato: {}", trunc); // Troncato: 2

    // Al multiplo più vicino di una potenza di dieci
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Arrotondato a 2 cifre decimali: {}", multiple_of_ten); // Arrotondato a 2 cifre decimali: 2.35
}
```

## Approfondimento
Storicamente, l'arrotondamento è stato cruciale per adattare decimali infiniti o numeri irrazionali in spazi digitali limitati — un must per gli antichi computer con memoria scarsa. Pensate all'abaco ma meno artigianale, più matematico.

Alternative ai metodi nativi di Rust includono:
1. la macro `format!` per la formattazione delle stringhe che arrotonda per default.
2. Crate esterni per compiti matematici specializzati, come il crate `round` con controllo più granulare.

Sotto il cofano, le operazioni di arrotondamento di Rust sono conformi agli standard IEEE — gergo tecnico per "arrotonda come vuole il tuo insegnante di matematica". Inoltre, a causa delle rappresentazioni binarie, alcuni numeri non possono essere arrotondati tradizionalmente, come lo 0,1, a causa della loro rappresentazione infinita in binario.

## Vedi Anche
- Documentazione Rust sui metodi del tipo primitivo: https://doc.rust-lang.org/std/primitive.f64.html
- Standard IEEE per l'aritmetica in virgola mobile (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Crate "round" per arrotondamenti più complessi: https://crates.io/crates/round
