---
title:                "Zapisanie wielkich liter w ciągu znaków"
html_title:           "Rust: Zapisanie wielkich liter w ciągu znaków"
simple_title:         "Zapisanie wielkich liter w ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

"## Co i dlaczego?"
Kapitalizacja ciągu znaków to proces zmiany pierwszego znaku w każdym wyrazie w ciągu na wielką literę. Programiści często wykonują tę akcję w celu poprawienia czytelności tekstu.

"## Jak to zrobić?"
Aby kapitalizować ciąg znaków w Rust, można użyć metody ```to_uppercase()```. Przykład:

```Rust
let string = "witajcie programiści";
let capitalized_string = string.to_uppercase();

println!("{}", capitalized_string);```

Wyjście:
WITAJCIE PROGRAMIŚCI

"## Deep Dive"
W przeszłości, kapitalizacja znaków była powszechnie stosowana w celu oznaczenia ważnych wyrazów lub tytułów. Jednak obecnie jest to częściej używane do poprawy czytelności i estetyki tekstu.

Alternatywnym podejściem jest używanie metody ```to_capitalized()``` lub używanie modułu ```unicode_normalization``` do kapitalizacji znaków. Wewnętrznie, kapitalizacja znaków w Rust jest oparta na funkcji ```to_uppercase()```, która przekształca znaki na ich odpowiednie duże odpowiedniki zgodnie z ustandaryzowanymi regułami dla języka utf8.

"## Zobacz również"
- Dokumentacja Rust dla metody ```to_uppercase()```: https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase
- Przykład z użyciem modułu ```unicode_normalization```: https://gist.github.com/wezm/5418291