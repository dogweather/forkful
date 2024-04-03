---
date: 2024-01-20 17:48:16.742453-07:00
description: "How to: Jak to zrobi\u0107."
lastmod: '2024-03-13T22:44:35.175193-06:00'
model: gpt-4-1106-preview
summary: "Jak to zrobi\u0107."
title: "Znalezienie d\u0142ugo\u015Bci ci\u0105gu znak\xF3w"
weight: 7
---

## How to:
Jak to zrobić:

```Rust
fn main() {
    let greeting = "Witaj, świecie!";
    let length = greeting.chars().count(); // Liczymy znaki, uwzględniając Unicode

    println!("Długość napisu '{}': {}", greeting, length);
}

// Wyjście:
// Długość napisu 'Witaj, świecie!': 15
```

## Deep Dive
Pogłębiona wiedza: Funkcja `len()` w Rust dostarcza liczbę bajtów w ciągu, co jest szybkie, ale nie działa dobrze z Unicode. Dlatego `chars().count()` to lepsza metoda, gdy pracujemy z Unicode -- liczy znaki, nie bajty. Kiedyś w C, liczenie długości łańcucha polegało na iterowaniu do znaku końca (NULL terminator). W Rust, długość łańcucha w bajtach jest znana natychmiast dzięki zapamiętanej wielkości, ale z Unicode to nie wystarcza.

## See Also
Zobacz również:

- Dokumentacja Rust'a na temat typów łańcuchowych: [String and str in Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Unicode w Rust: [Understanding Unicode in Rust](https://blog.rust-lang.org/2020/01/03/reducing-support-for-32-bit-apple-targets.html)
