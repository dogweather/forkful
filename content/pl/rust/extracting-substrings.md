---
title:                "Ekstrakcja podciągów"
html_title:           "Rust: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co & dlaczego?

Wycinanie podciągów (czyli małych fragmentów) z tekstu jest częstym zadaniem w programowaniu. Programiści często wykorzystują to do wyodrębnienia konkretnych informacji z dłuższych ciągów znaków, co ułatwia przetwarzanie danych.

## Jak to zrobić:

### Wycinanie podciągów zaczynających się od konkretnego indeksu

```Rust
let text = "To jest przykładowy tekst";
let subtext = &text[8..];
println!("{}", subtext);
```

Wynik: `przykładowy tekst`

### Wycinanie podciągów o określonej długości

```Rust
let text = "12345";
let subtext = &text[..2];
println!("{}", subtext);
```

Wynik: `12`

## Głębsze spojrzenie:

### Kontekst historyczny

Wycinanie podciągów jest powszechne w wielu językach programowania. Pierwsze algorytmy do przetwarzania tekstu i wyodrębniania podciągów pojawiły się już w latach 60. W dzisiejszych czasach jest to podstawowa operacja w praktycznie każdym języku programowania.

### Alternatywy

W języku Rust wycinanie podciągów można również wykonać przy użyciu metody `split_at()` lub używając biblioteki `regex`.

### Szczegóły implementacji

Wycinanie podciągów w języku Rust jest oparte na indeksach typu `usize`. Najważniejsze operacje, takie jak sprawdzenie poprawności indeksów czy zwrócenie fragmentu tekstu, są wykonywane poprzez wykorzystanie metod typu `slice`.

## Zobacz też:

- [Dokumentacja Rust](https://doc.rust-lang.org/std/primitive.slice.html)
- [Tutorial o wycinaniu podciągów w języku Rust](https://www.tutorialspoint.com/rust/rust_string_slice.htm)
- [Alternatywne metody wycinania podciągów w języku Rust](https://doc.rust-lang.org/std/primitive.slice.html#methods)