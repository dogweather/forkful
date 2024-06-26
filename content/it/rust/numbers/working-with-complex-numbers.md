---
date: 2024-01-26 04:45:19.054577-07:00
description: 'Come fare: Rust non ha supporto nativo per i numeri complessi, ma crate
  come `num-complex` vi coprono le spalle. Ecco come usarlo.'
lastmod: '2024-03-13T22:44:43.212084-06:00'
model: gpt-4-0125-preview
summary: Rust non ha supporto nativo per i numeri complessi, ma crate come `num-complex`
  vi coprono le spalle.
title: Lavorare con i numeri complessi
weight: 14
---

## Come fare:
Rust non ha supporto nativo per i numeri complessi, ma crate come `num-complex` vi coprono le spalle. Ecco come usarlo:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let somma = a + b;
    let prodotto = a * b;

    println!("Somma: {}", somma); // Somma: 3 - 1i
    println!("Prodotto: {}", prodotto); // Prodotto: 14 - 5i
}
```
Dovrete aggiungere `num_complex` al vostro `Cargo.toml` per far accadere questa magia.

## Approfondimento
I numeri complessi sono stati concepiti nel XVI secolo ma hanno realmente preso piede nel XVIII secolo quando matematici come Eulero hanno iniziato a giocarci. 

Senza operazioni native sui numeri complessi, linguaggi come Rust si affidano a librerie di terze parti. `num-complex` è una di queste crate ed è parte della collezione di crate `num` che mira a fornire tipi numerici e tratti per Rust.

Vale la pena menzionare che alcuni linguaggi (come Python) hanno un supporto integrato per i numeri complessi, mentre altri (come C++, con l'header `<complex>`) li forniscono come parte della libreria standard. In Rust, la decisione di mantenere la libreria standard piccola significa che spesso si ricorre a crate create dalla comunità per funzionalità aggiuntive.

## Vedi Anche
- [Libro di Rust](https://doc.rust-lang.org/book/): Per saperne di più su Rust e come lavorare con crate esterne.
- [Numero Complesso Wikipedia](https://en.wikipedia.org/wiki/Complex_number): Per una comprensione più approfondita dei numeri complessi.
