---
title:                "Zapisywane wielkich liter łańcucha"
html_title:           "Rust: Zapisywane wielkich liter łańcucha"
simple_title:         "Zapisywane wielkich liter łańcucha"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, gdy coraz więcej aplikacji wymaga wprowadzania tekstu, często konieczne jest formatowanie tego tekstu, aby wyglądał estetycznie i czytelnie. Istnieją różne sposoby na formatowanie tekstu, jednym z nich jest właśnie kapitalizowanie ciągów znaków. W tym artykule dowiesz się, jak to zrobić w języku Rust.

## Jak to zrobić

Aby kapitalizować ciąg znaków w Rust, możesz skorzystać z metody `to_uppercase` z modułu `std::string`.

```Rust
let my_string = "hello world";
let capitalized_string = my_string.to_uppercase();

println!("{}", capitalized_string);
```

W powyższym przykładzie, tworzymy zmienną `my_string` zawierającą tekst "hello world", a następnie przypisujemy kapitalizowany ciąg znaków do zmiennej `capitalized_string` wykorzystując metodę `to_uppercase`. W końcu, wyświetlamy wynik za pomocą funkcji `println`.

Możemy także użyć metody `capitalize` z modułu `str` do kapitalizowania tylko pierwszej litery w ciągu znaków.

```Rust
let my_string = "hello world";
let capitalized_first_letter = my_string.capitalize();

println!("{}", capitalized_first_letter);
```

W wyniku powyższego przykładu, otrzymamy "Hello world" zamiast "HELLO WORLD". Jest to przydatne, gdy chcemy zachować oryginalny wygląd reszty tekstu, ale jednocześnie chcemy, aby pierwsza litera była duża.

## Deep Dive

Podczas kapitalizowania ciągu znaków, warto pamiętać o różnych językach i ich sposobie obsługi wielkich liter. W językach niektórych językach, jak na przykład w języku tureckim, istnieją specjalne znaki, które mogą zmienić całe słowo przy użyciu kapitalizacji, więc warto zwrócić na to uwagę.

Ponadto, warto zauważyć, że metody `to_uppercase` i `capitalize` zwracają nowe ciągi znaków, a nie modyfikują oryginalnego. Jeśli chcesz zachować zmodyfikowaną wersję, musisz ją przypisać do nowej zmiennej.

## Zobacz także

- [Dokumentacja Rust na temat kapitalizacji](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Poradnik na temat pracy z tekstami w Rust](https://www.asquera.de/blog/2015-07-03/working-with-strings-in-rust/)