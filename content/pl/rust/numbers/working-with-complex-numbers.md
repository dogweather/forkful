---
date: 2024-01-26 04:45:43.023442-07:00
description: "Jak to zrobi\u0107: Rust nie posiada wbudowanego wsparcia dla liczb\
  \ z\u0142o\u017Conych, ale takie pakiety jak `num-complex` przychodz\u0105 z pomoc\u0105\
  . Oto jak z nich\u2026"
lastmod: '2024-03-13T22:44:35.178358-06:00'
model: gpt-4-0125-preview
summary: "Rust nie posiada wbudowanego wsparcia dla liczb z\u0142o\u017Conych, ale\
  \ takie pakiety jak `num-complex` przychodz\u0105 z pomoc\u0105."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
Rust nie posiada wbudowanego wsparcia dla liczb złożonych, ale takie pakiety jak `num-complex` przychodzą z pomocą. Oto jak z nich korzystać:

```rust
use num_complex::Complex;

fn main() {
    let a = Complex::new(2.0, 3.0); // 2 + 3i
    let b = Complex::new(1.0, -4.0); // 1 - 4i

    let suma = a + b;
    let produkt = a * b;

    println!("Suma: {}", suma); // Suma: 3 - 1i
    println!("Produkt: {}", produkt); // Produkt: 14 - 5i
}
```
Aby to wszystko działało, musisz dodać `num_complex` do swojego pliku `Cargo.toml`.

## Wnikliwe spojrzenie
Liczby złożone zostały zapoczątkowane w XVI wieku, ale naprawdę zyskały na popularności w XVIII wieku, kiedy matematycy tacy jak Euler zaczęli się nimi bawić.

Bez natywnego wsparcia dla operacji na liczbach złożonych, języki takie jak Rust polegają na bibliotekach stron trzecich. `num-complex` jest jednym z takich pakietów i jest częścią kolekcji pakietów `num`, która ma na celu dostarczenie typów numerycznych i cech dla Rust.

Warto wspomnieć, że niektóre języki (takie jak Python) mają wbudowane wsparcie dla liczb złożonych, podczas gdy inne (jak C++ z nagłówkiem `<complex>`) dostarczają je jako część standardowej biblioteki. W Rust, decyzja o utrzymaniu małej standardowej biblioteki oznacza, że często sięgamy po pakiety tworzone przez społeczność dla dodatkowej funkcjonalności.

## Zobacz również
- [Książka o Rust](https://doc.rust-lang.org/book/): Aby dowiedzieć się więcej o Rust i jak pracować z zewnętrznymi pakietami.
- [Wikipedia Liczba Złożona](https://pl.wikipedia.org/wiki/Liczba_z%C5%82o%C5%BCona): Aby lepiej zrozumieć liczby złożone.
