---
date: 2024-01-26 03:47:05.989001-07:00
description: "Jak to zrobi\u0107: Rust sprawia, \u017Ce zaokr\u0105glanie jest bardzo\
  \ proste. Sprawd\u017A te metody dla typ\xF3w `f32` lub `f64`."
lastmod: '2024-03-13T22:44:35.179298-06:00'
model: gpt-4-0125-preview
summary: "Rust sprawia, \u017Ce zaokr\u0105glanie jest bardzo proste."
title: "Zaokr\u0105glanie liczb"
weight: 13
---

## Jak to zrobić:
Rust sprawia, że zaokrąglanie jest bardzo proste. Sprawdź te metody dla typów `f32` lub `f64`:

```rust
fn main() {
    let num = 2.34567;

    // Zaokrąglenie do najbliższej liczby całkowitej
    let round = num.round();
    println!("Zaokrąglone: {}", round); // Zaokrąglone: 2

    // Zaokrąglenie w dół - największa liczba całkowita mniejsza lub równa liczbie
    let floor = num.floor();
    println!("Podłoga: {}", floor); // Podłoga: 2

    // Zaokrąglenie w górę - najmniejsza liczba całkowita większa lub równa liczbie
    let ceil = num.ceil();
    println!("Sufit: {}", ceil); // Sufit: 3

    // Obcięcie - część całkowita bez cyfr ułamkowych
    let trunc = num.trunc();
    println!("Obcięte: {}", trunc); // Obcięte: 2

    // Do najbliższej wielokrotności potęgi dziesięciu
    let multiple_of_ten = (num * 100.0).round() / 100.0;
    println!("Zaokrąglone do 2 miejsc po przecinku: {}", multiple_of_ten); // Zaokrąglone do 2 miejsc po przecinku: 2.35
}
```

## Szczegółowa analiza
Historycznie, zaokrąglanie było kluczowe do dopasowywania nieskończonych ułamków dziesiętnych lub liczb niewymiernych w ograniczonej przestrzeni cyfrowej - konieczność dla starożytnych komputerów z ograniczoną pamięcią. Pomyśl o abakusie, ale mniej wymyślnie, bardziej matematycznie.

Alternatywy dla natywnych metod Rusta obejmują:
1. Makro `format!` do formatowania ciągów znaków, które domyślnie zaokrągla.
2. Zewnętrzne skrzynki (crates) do specjalistycznych zadań matematycznych, takie jak skrzynka `round` z bardziej szczegółową kontrolą.

W głębi, operacje zaokrąglania w Rust spełniają standardy IEEE - żargon techniczny oznaczający "zaokrągla tak, jak chce tego twój nauczyciel matematyki". Plus, ze względu na reprezentacje binarne, niektóre liczby nie mogą być tradycyjnie zaokrąglone, jak 0,1, z powodu ich nieskończonej reprezentacji w binarnym.

## Zobacz także
- Dokumentacja Rusta o metodach typów prymitywnych: https://doc.rust-lang.org/std/primitive.f64.html
- Standard IEEE dla arytmetyki zmiennoprzecinkowej (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Skrzynka `round` do bardziej skomplikowanego zaokrąglania: https://crates.io/crates/round
