---
title:                "Wyszukiwanie i zamiana tekstu"
aliases:
- /pl/rust/searching-and-replacing-text/
date:                  2024-01-20T17:58:41.582020-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wyszukiwanie i zamiana tekstu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/searching-and-replacing-text.md"
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
