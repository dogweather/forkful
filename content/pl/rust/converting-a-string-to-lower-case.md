---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Konwersja tekstu na małe litery to prosty proces zamiany wszystkich znaków alfabetycznych w ciągu na ich odpowiedniki w małych literach. Programiści robią to, aby uniknąć błędów związanych z różnicami w wielkości liter, ułatwić porównywanie tekstu oraz zoptymalizować działanie algorytmów.

## Jak to zrobić:
Oto jak to zrobisz w Rust:

```rust
fn main() {
    let s = "Witaj, Świecie!";
    println!("{}", s.to_lowercase());
}
```

Po uruchomieniu programu na ekranie pojawi się:

```
witaj, świecie!
```

## Więcej informacji
Historia: Konwersja na małe litery jest podstawowym narzędziem w wielu językach programowania. Pojawia się prawie we wszystkich, począwszy od pierwszych języków, takich jak FORTRAN czy COBOL, do najnowszych, takich jak Python, JavaScript, czy rust.

Alternatywy: W rust, dodatkowo do `to_lowercase()`, jest też metoda `to_ascii_lowercase()`, która działa tylko dla ASCII.

Szczegóły implementacji: W rust metoda `to_lowercase()` działa na poziomie jednego znaku. Oznacza to, że konwertuje każdy pojedynczy znak z ciągu niezależnie. To z kolei oznacza, że nie jest ona idealna dla wszystkich języków - niektóre mają specjalne reguły dotyczące konwersji na małe litery, których rust nie obsługuje.

## Zobacz też
1. Dokumentacja rust do `to_lowercase()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase
2. Stack overflow - Porównywanie tekstu ignorując wielkość liter: https://stackoverflow.com/questions/2135912/do-case-insensitive-string-comparison-in-rust