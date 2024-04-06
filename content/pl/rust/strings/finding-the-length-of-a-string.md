---
date: 2024-01-20 17:48:16.742453-07:00
description: "How to: Pog\u0142\u0119biona wiedza: Funkcja `len()` w Rust dostarcza\
  \ liczb\u0119 bajt\xF3w w ci\u0105gu, co jest szybkie, ale nie dzia\u0142a dobrze\
  \ z Unicode. Dlatego\u2026"
lastmod: '2024-04-05T22:50:49.479619-06:00'
model: gpt-4-1106-preview
summary: "Pog\u0142\u0119biona wiedza: Funkcja `len()` w Rust dostarcza liczb\u0119\
  \ bajt\xF3w w ci\u0105gu, co jest szybkie, ale nie dzia\u0142a dobrze z Unicode.\
  \ Dlatego `chars().count()` to lepsza metoda, gdy pracujemy z Unicode -- liczy znaki,\
  \ nie bajty. Kiedy\u015B w C, liczenie d\u0142ugo\u015Bci \u0142a\u0144cucha polega\u0142\
  o na iterowaniu do znaku ko\u0144ca (NULL terminator). W Rust, d\u0142ugo\u015B\u0107\
  \ \u0142a\u0144cucha w bajtach jest znana natychmiast dzi\u0119ki zapami\u0119tanej\
  \ wielko\u015Bci, ale z Unicode to nie wystarcza."
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
