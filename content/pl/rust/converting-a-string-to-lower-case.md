---
title:                "Zmiana ciągu znaków na małe litery"
html_title:           "Rust: Zmiana ciągu znaków na małe litery"
simple_title:         "Zmiana ciągu znaków na małe litery"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co to jest i po co? 
Konwersja ciągu znaków na małe litery jest procesem zmiany wszystkich znaków w ciągu na ich odpowiedniki w małych literach. Programiści często wykonują tę operację w celu ujednolicenia ciągów znaków, aby łatwiej było je porównywać lub przetwarzać.

## Jak to zrobić: 
```Rust
let my_word = "RUST";
let lower_case_word = my_word.to_lowercase();
println!("{}", lower_case_word);
```
Output:
```Rust
rust
```

## Wnikliwe spojrzenie: 
Konwersja ciągu znaków na małe litery jest powszechnie stosowana w programowaniu oraz w życiu codziennym. W przeszłości, gdy komputery były w stanie obsługiwać tylko wielkie litery, konwersja na małe litery była często wykorzystywana do ułatwienia przetwarzania tekstu. Jedną z alternatyw dla funkcji to_lowercase() w języku Rust jest używanie metody iteracji po ciągu znaków i zmiany ich na małe litery za pomocą metody to_ascii_lowercase(). W językach programowania, które nie posiadają wbudowanej funkcji do konwersji na małe litery, można użyć funkcji dostępnych w bibliotekach zewnętrznych lub napisać własną implementację.

## Zobacz również: 
- [Dokumentacja języka Rust o konwersji ciągu na małe litery](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Porównanie wydajności różnych metod konwersji ciągu na małe litery w języku Rust](https://www.joshmatthews.net/polish-app/)
- [Przydatne narzędzia do pracy z ciągami znaków w języku Rust](https://lib.rs/collections/string_utilities)