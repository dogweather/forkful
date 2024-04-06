---
date: 2024-01-20 17:46:44.636267-07:00
description: "How to (Jak to zrobi\u0107): Wskaz\xF3wka: Uwa\u017Caj na Unicode! `chars()`\
  \ iteruje po warto\u015Bciach znak\xF3w, a nie po bajtach."
lastmod: '2024-04-05T21:53:36.610677-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to (Jak to zrobić):
```Rust
fn main() {
    let text = "Jak to zrobić w Rust?";
    let start = 8;
    let end = 15;
    
    // Podejście 1: Używając metody `get`
    match text.get(start..end) {
        Some(substring) => println!("Podejście 1: {}", substring),
        None => println!("Podejście 1: Wybrano nieprawidłowy zakres!"),
    }

    // Podejście 2: Slices and the `chars()` iterator
    let substring: String = text.chars().skip(start).take(end - start).collect();
    println!("Podejście 2: {}", substring);
}

// Sample output:
// Podejście 1: zrobić
// Podejście 2: zrobić
```

Wskazówka: Uważaj na Unicode! `chars()` iteruje po wartościach znaków, a nie po bajtach.

## Deep Dive (Głębsze nurkowanie):
Historia czasami wpływa na to, jak kodujemy. Rust został stworzony, by zarządzać pamięcią bezpiecznie i efektywnie. Znane są metody `.slice()`, które omijały te cechy Rusta, ale z czasem metody te zostały zamienione na bardziej bezpieczne `get()` i iteratory.

Jeśli chodzi o alternatywy, istnieje także crate `regex` dla przypadków wymagających bardziej skomplikowanego wyszukiwania. Implementacja jest ważna z powodu sposobu, w jaki Rust obsługuje stringi jak ciągi bajtów, a nie ciągi znaków, co ma znaczenie dla Unicode. Slices pozwalają na łatwe wyodrębnianie, ale należy pamiętać o granicach kodowania znaków.

## See Also (Zobacz także):
- Oficjalna dokumentacja String i str w Rust: [std::string](https://doc.rust-lang.org/std/string/index.html), [std::str](https://doc.rust-lang.org/std/str/)
