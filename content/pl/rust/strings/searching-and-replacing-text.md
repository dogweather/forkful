---
date: 2024-01-20 17:58:41.582020-07:00
description: "How to: (Jak to zrobi\u0107:) Historia funkcji wyszukiwania i zamiany\
  \ si\u0119ga pocz\u0105tku edycji tekstu na komputerach. Alternatyw\u0105 dla `replace()`\
  \ w Rust jest\u2026"
lastmod: '2024-04-05T22:50:49.473038-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Historia funkcji wyszukiwania i zamiany si\u0119ga\
  \ pocz\u0105tku edycji tekstu na komputerach."
title: Wyszukiwanie i zamiana tekstu
weight: 10
---

## How to: (Jak to zrobić:)
```Rust
fn main() {
    let text = "Witaj, Rust!".to_string();
    let search = "Rust";
    let replace_with = "Świat";

    let updated_text = text.replace(search, replace_with);
    println!("{}", updated_text);
}
```
Wyjście:
```
Witaj, Świat!
```

## Deep Dive (Głębsze spojrzenie)
Historia funkcji wyszukiwania i zamiany sięga początku edycji tekstu na komputerach. Alternatywą dla `replace()` w Rust jest użycie wyrażeń regularnych z crate'a `regex` - to daje więcej mocy, ale jest też kosztowniejsze obliczeniowo. Aby zamienić tekst w Rust, można użyć wbudowanej metody `replace()` lub zbudować własną funkcję, co może być optymalne dla specyficznych przypadków.

## See Also (Zobacz również)
- [Rust documentation for 'str'](https://doc.rust-lang.org/std/primitive.str.html)
- [Rust's Regex Crate Documentation](https://docs.rs/regex/)
- [The Book 'Programming Rust'](https://doc.rust-lang.org/book/)
