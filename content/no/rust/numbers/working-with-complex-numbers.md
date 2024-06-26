---
date: 2024-01-26 04:45:28.128978-07:00
description: "Hvordan: Rust har ikke innebygd st\xF8tte for komplekse tall, men pakker\
  \ som `num-complex` har ryggen din. Her er hvordan du bruker den."
lastmod: '2024-03-13T22:44:40.567938-06:00'
model: gpt-4-0125-preview
summary: "Rust har ikke innebygd st\xF8tte for komplekse tall, men pakker som `num-complex`\
  \ har ryggen din."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Rust har ikke innebygd støtte for komplekse tall, men pakker som `num-complex` har ryggen din. Her er hvordan du bruker den:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let produkt = a * b;

    println!("Sum: {}", sum); // Sum: 3 - 1i
    println!("Produkt: {}", produkt); // Produkt: 14 - 5i
}
```
Du må legge til `num_complex` i din `Cargo.toml` for å få til denne magien.

## Dypdykk
Komplekse tall ble tenkt ut på 16-tallet, men tok virkelig av på 18-tallet da matematikere som Euler begynte å eksperimentere med dem.

Uten innebygde komplekse tall operasjoner, stoler språk som Rust på tredjepartsbiblioteker. `num-complex` er en slik pakke og er en del av `num` pakkesamlingen som sikter på å tilby numeriske typer og trekk for Rust.

Det er verdt å nevne at noen språk (som Python) har innebygd støtte for komplekse tall, mens andre (som C++, med `<complex>` headeren) tilbyr dem som en del av standardbiblioteket. I Rust, beslutningen om å holde standardbiblioteket lite betyr at du ofte vil strekke deg etter fellesskapsopprettede pakker for ekstra funksjonalitet.

## Se Også
- [Rust Book](https://doc.rust-lang.org/book/): For å lære mer om Rust og hvordan jobbe med eksterne pakker.
- [Komplekst Tall Wikipedia](https://en.wikipedia.org/wiki/Complex_number): For en dypere forståelse av komplekse tall.
