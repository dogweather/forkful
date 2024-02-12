---
title:                "Å jobbe med komplekse tall"
date:                  2024-01-26T04:45:28.128978-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med komplekse tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/rust/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Komplekse tall har en reell del og en imaginær del og er essensielle i forskjellige felt som ingeniørvitenskap, fysikk, og datagrafikk. Programmerere bruker dem til å løse ligninger som vanlige reelle tall ikke kan håndtere.

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
