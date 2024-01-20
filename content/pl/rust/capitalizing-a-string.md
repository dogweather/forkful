---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "Rust: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zmiana rozkładu liter na duże (tzw. wielkie litery) to proces, w którym pierwsza litera każdego słowa w ciągu jest zamieniana na wielką literę. Programiści robią to, aby dane wejściowe były bardziej czytelne dla użytkowników.

## Jak to zrobić:

Rust ma wiele metod do przekształcania ciągów w wielkie litery. Najprostszy to `.to_uppercase()` na ciągu. Sprawdźmy to.

```Rust
fn main() {
    let small_str = "cześć, świecie";
    let big_str = small_str.to_uppercase();
    println!("{}", big_str);
}
```
Po uruchomieniu powyższego kodu, wyjście powinno wyglądać tak:

```Rust
"CZEŚĆ, ŚWIECIE"
```

## W Głąb:

Innym sposobem manipulacji wielkością liter na dużą w Rust jest użycie standardowej biblioteki `unicode_segmentation` i metody `to_titlecase`, która pozwala zamieniać pierwszą literę każdego wyrazu na duże litery, zamiast konwertować cały ciąg. 

```Rust
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let s = "witaj, świecie".to_string();
    let title_s = s.unicode_words().map(|w| w.chars().next().unwrap().to_uppercase() + &w.chars().skip(1).collect::<String>()).collect::<Vec<_>>().join(" ");
    println!("{}", title_s);
}
```

## Zobacz Też:

1. Dokumentacja Rust do metody `to_uppercase`: https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase

2. Dokumentacja Rust do metody `to_titlecase`: https://unicode-rs.github.io/unicode-segmentation/unicode_segmentation/trait.UnicodeSegmentation.html#method.unicode_words