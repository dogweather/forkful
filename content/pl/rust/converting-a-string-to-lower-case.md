---
title:    "Rust: Konwertowanie ciągu znaków na małe litery"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery może być bardzo przydatna podczas programowania w języku Rust. Dzięki temu możemy upewnić się, że porównywane teksty są równe, niezależnie od tego czy zawierają duże lub małe litery. Jest to także ważne w przypadku pracy z danymi użytkownika, ponieważ zapewnia spójne działanie niezależnie od tego jak wpisane zostaną dane.

## Jak to zrobić

Aby przekonwertować tekst na małe litery w Rust, możemy użyć metody `.to_lowercase()` na obiekcie typu `String`. Przykładowe użycie wyglądałoby następująco:

```Rust
let name = "JAN KOWALSKI";
let lowercase_name = name.to_lowercase();
println!("Witaj, {}", lowercase_name); // Output: "Witaj, jan kowalski"
```

W przypadku wartości zmiennoprzecinkowych, które nie mają metody `.to_lowercase()`, możemy użyć biblioteki `unicode-normalization` i funkcji `Chars()` aby przetworzyć każdy znak z osobna. Przykład:

```Rust
let float_num = 99.99;
let lowercase_num = format!("{:?}", float_num)
    .chars()
    .flat_map(|ch| ch.to_lowercase())
    .collect::<String>();
println!("{}", lowercase_num); // Output: "99.99"
```

## Deep Dive

W języku Rust istnieją także funkcje związane z konwersją tekstu na duże litery (`to_uppercase()`) oraz pierwszą literę każdego słowa na dużą (`to_titlecase()`). W przypadku kodowania ścieżek do plików, dostępna jest także funkcja `to_ascii_lowercase()` dla zachowania spójności z systemem plików.

Podczas konwersji na małe litery, niektóre znaki mogą ulec zmianie, na przykład niemieckie litery `ß` i `ẞ` będą przekonwertowane na `ss` i `ẞ` odpowiednio. Jest to ważne do uwzględnienia podczas pracy z danymi zawierającymi specjalne znaki.

## Zobacz również

- Dokumentacja języka Rust dotycząca metod `to_lowercase()`, `to_uppercase()` i `to_titlecase()`: https://doc.rust-lang.org/std/string/trait.ToString.html
- Biblioteka `unicode-normalization` dla konwersji znaków: https://crates.io/crates/unicode-normalization
- Przykładowe zastosowanie konwersji na małe litery: https://www.dotnetperls.com/to-lowercase-rust