---
date: 2024-01-26 04:45:30.749988-07:00
description: "Hur: Rust har inte inbyggt st\xF6d f\xF6r komplexa tal, men paket som\
  \ `num-complex` t\xE4cker dina behov. S\xE5 h\xE4r anv\xE4nder du det."
lastmod: '2024-03-13T22:44:37.691925-06:00'
model: gpt-4-0125-preview
summary: "Rust har inte inbyggt st\xF6d f\xF6r komplexa tal, men paket som `num-complex`\
  \ t\xE4cker dina behov."
title: Att arbeta med komplexa tal
weight: 14
---

## Hur:
Rust har inte inbyggt stöd för komplexa tal, men paket som `num-complex` täcker dina behov. Så här använder du det:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let sum = a + b;
    let produkt = a * b;

    println!("Summa: {}", sum); // Summa: 3 - 1i
    println!("Produkt: {}", produkt); // Produkt: 14 - 5i
}
```
Du måste lägga till `num_complex` i din `Cargo.toml` för att få detta att hända.

## Fördjupning
Komplexa tal uppfanns under 1500-talet men tog verkligen fart under 1700-talet när matematiker som Euler började experimentera med dem.

Utan inbyggda operationer för komplexa tal, förlitar sig språk som Rust på tredjepartsbibliotek. `num-complex` är ett sådant paket och är en del av `num`-paketkollektionen som syftar till att tillhandahålla numeriska typer och egenskaper för Rust.

Det är värt att nämna att vissa språk (som Python) har inbyggt stöd för komplexa tal, medan andra (som C++, med `<complex>`-huvudfilen) tillhandahåller dem som en del av standardbiblioteket. I Rust innebär beslutet att hålla standardbiblioteket litet att du ofta söker dig till samhällsskapade paket för ytterligare funktionalitet.

## Se även
- [Rust Book](https://doc.rust-lang.org/book/): För att lära dig mer om Rust och hur man arbetar med externa paket.
- [Komplexa tal Wikipedia](https://sv.wikipedia.org/wiki/Komplext_tal): För en djupare förståelse av komplexa tal.
