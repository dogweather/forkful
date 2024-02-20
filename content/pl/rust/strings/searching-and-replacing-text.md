---
date: 2024-01-20 17:58:41.582020-07:00
description: "Wyszukiwanie i zamiana tekstu to standardowe akcje przetwarzaj\u0105\
  ce \u0142a\u0144cuchy znak\xF3w - znajdujemy fragment tekstu i podmieniamy go na\
  \ inny. Programi\u015Bci robi\u0105\u2026"
lastmod: 2024-02-19 22:04:54.295372
model: gpt-4-1106-preview
summary: "Wyszukiwanie i zamiana tekstu to standardowe akcje przetwarzaj\u0105ce \u0142\
  a\u0144cuchy znak\xF3w - znajdujemy fragment tekstu i podmieniamy go na inny. Programi\u015B\
  ci robi\u0105\u2026"
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wyszukiwanie i zamiana tekstu to standardowe akcje przetwarzające łańcuchy znaków - znajdujemy fragment tekstu i podmieniamy go na inny. Programiści robią to, by modyfikować dane, configi czy kod - to takie cyfrowe "znajdź i zastąp" z edytora tekstu.

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
