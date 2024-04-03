---
date: 2024-01-20 17:46:44.636267-07:00
description: "Wyodr\u0119bnianie podci\u0105g\xF3w to dzia\u0142anie polegaj\u0105\
  ce na przyw\u0142aszczeniu sobie cz\u0119\u015Bci stringu. Programi\u015Bci robi\u0105\
  \ to, aby pracowa\u0107 tylko z tymi z\u0142otymi kawa\u0142kami\u2026"
lastmod: '2024-03-13T22:44:35.173113-06:00'
model: gpt-4-1106-preview
summary: "Wyodr\u0119bnianie podci\u0105g\xF3w to dzia\u0142anie polegaj\u0105ce na\
  \ przyw\u0142aszczeniu sobie cz\u0119\u015Bci stringu."
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
