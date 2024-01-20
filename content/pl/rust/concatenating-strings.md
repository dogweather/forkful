---
title:                "Łączenie ciągów znaków"
html_title:           "Arduino: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Łączenie łańcuchów znaków to proces, w którym dwie lub więcej wartości tekstowych łączy się w jeden dłuższy ciąg. Programiści przeprowadzają to między innymi dla łatwiejszego formatowania i prezentowania danych.

## Jak to zrobić:
Rust oferuje kilka sposobów na łączenie ciągów znaków. Najpopularniejsze z nich to użycie operatora `+` lub funkcji `format!`.

```Rust
let hello = "Cześć";
let world = "świecie";
let message = hello.to_string() + " " + world;
println!("{}", message); 
```
Wyjście:
```
Cześć świecie
```
Alternatywnie, można użyć funkcji `format!`, która umożliwia składnię podobną do `printf` w języku C.
```Rust
let hello = "Cześć";
let world = "świecie";
let message = format!("{} {}", hello, world);
println!("{}", message);
```
Wyjście:
```
Cześć świecie
```

## Deep Dive
Historia: Operator `+` jest obecny w wielu językach programowania, a Rust go przyjął dla spójności i łatwości zrozumienia. Funkcja `format!` pochodzi z rodziny języków C, w której `printf` pełni tę samą rolę.

Alternatywy: Oprócz operatora `+` i `format!` Rust oferuje również metody `push_str` i `push`, które są wydajniejsze, szczególnie, gdy łączy się wiele ciągów.

Szczegóły implementacji: Łączenie ciągów w Rust jest kosztowne pod kątem wydajności, podobnie jak w innych językach. Rust kładzie duży nacisk na bezpieczeństwo i zasoby, dlatego nie pozwala na modyfikację ciągów, co z kolei prowadzi do tworzenia nowego ciągu przy połączeniu.

## Zobacz także
Check the Rust documentation for more examples and usage:
Sprawdź dokumentację Rust, aby uzyskać więcej przykładów i zastosowań:
- [`std::string::String`](https://doc.rust-lang.org/std/string/struct.String.html) 
- [`std::fmt`](https://doc.rust-lang.org/std/fmt/) 

Możesz również konsultować te źródła dla dogłębnej analizy:
- [The Rust Book](https://doc.rust-lang.org/book/ch08-02-strings.html) 
- [Efficient String Concatenation in Rust](https://fasterthanli.me/series/efficient-string-concatenation-in-rust)