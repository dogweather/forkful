---
date: 2024-01-26 04:45:43.023442-07:00
description: "Liczby z\u0142o\u017Cone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015B\
  ci rzeczywistej i urojonej i s\u0105 niezb\u0119dne w r\xF3\u017Cnych dziedzinach,\
  \ takich jak in\u017Cynieria, fizyka i grafika komputerowa.\u2026"
lastmod: 2024-02-19 22:04:54.304884
model: gpt-4-0125-preview
summary: "Liczby z\u0142o\u017Cone sk\u0142adaj\u0105 si\u0119 z cz\u0119\u015Bci\
  \ rzeczywistej i urojonej i s\u0105 niezb\u0119dne w r\xF3\u017Cnych dziedzinach,\
  \ takich jak in\u017Cynieria, fizyka i grafika komputerowa.\u2026"
title: Praca z liczbami zespolonymi
---

{{< edit_this_page >}}

## Co i dlaczego?
Liczby złożone składają się z części rzeczywistej i urojonej i są niezbędne w różnych dziedzinach, takich jak inżynieria, fizyka i grafika komputerowa. Programiści używają ich do rozwiązywania równań, z którymi zwykłe liczby rzeczywiste nie mogą sobie poradzić.

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
