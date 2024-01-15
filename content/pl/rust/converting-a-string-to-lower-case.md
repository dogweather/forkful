---
title:                "Konwertowanie ciągu znaków na małe litery"
html_title:           "Rust: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja tekstu na małe litery (lower case) jest częstym zadaniem w programowaniu, które może być potrzebne przy przetwarzaniu danych lub porównywaniu ciągów tekstu. W tym artykule dowiesz się, w jaki sposób dokonać takiej konwersji za pomocą języka Rust.

## Jak to zrobić

Konwersja tekstu na małe litery w języku Rust jest bardzo prosta i szybka. Można to zrobić za pomocą metody `to_lowercase()`, która jest dostępna dla typu `String` oraz `&str`. Przykładowy kod wyglądałby następująco:

```Rust
let text = "PROGRAMOWANIE RUST";
let lower_case_text = text.to_lowercase();
println!("{}", lower_case_text);
```

W powyższym przykładzie utworzyliśmy zmienną `text` zawierającą tekst w dużych literach, a następnie przy pomocy metody `to_lowercase()` przekształciliśmy go na tekst w małych literach. Wynik zostanie wyświetlony na ekranie jako `programowanie rust`.

## Głębsze wertowanie

W praktyce, podczas konwersji tekstu na małe litery może pojawić się kilka problemów, które warto znać. Jednym z nich jest porównywanie ciągów tekstowych różnej wielkości liter. Domyślnie funkcja `to_lowercase()` konwertuje tekst na małe litery na podstawie standardowego zestawu znaków Unicode. Dzięki temu możliwe jest porównywanie tekstów bez względu na wielkość liter. Jednak w niektórych przypadkach może to nie być oczekiwany efekt - na przykład w sytuacji, gdy chcemy zachować wielkość liter w pewnych częściach tekstu. Wtedy warto skorzystać z metody `to_lowercase()` z biblioteki `unicase`, która pozwala na bardziej elastyczną kontrolę nad konwersją tekstu.

## Zobacz również

- [Dokumentacja języka Rust - metoda to_lowercase()](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Biblioteka unicase](https://crates.io/crates/unicase)