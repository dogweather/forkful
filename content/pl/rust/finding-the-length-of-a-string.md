---
title:                "Znajdowanie długości łańcucha znaków"
html_title:           "Rust: Znajdowanie długości łańcucha znaków"
simple_title:         "Znajdowanie długości łańcucha znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Znalezienie długości ciągu znaków to podstawowa czynność w programowaniu. Polega ona na obliczeniu liczby znaków w danym ciągu, takich jak litery, cyfry i znaki specjalne. Programiści wykonują tę czynność, aby móc manipulować i analizować dane tekstowe.

## Jak to zrobić:

```Rust
let string = "Hello World!";
let len = string.len();
println!("Długość ciągu: {}", len);

// Output: Długość ciągu: 12
```

Możemy wykorzystać funkcję `len()` do obliczenia długości ciągu znaków w Rust. Ta funkcja jest dostępna dla typu `String` oraz dla typów, które implementują `std::ops::Index` trait.

```Rust
let name = String::from("Michael");
let len = name.len();
println!("Długość imienia: {}", len);

// Output: Długość imienia: 7
```

Możemy także wykorzystać metodę `chars()` do zliczenia liczby znaków w ciągu. Ta metoda zwraca iterator, dlatego musimy wykorzystać funkcję `count()` do zliczenia elementów.

```Rust
let message = "Witaj, użytkowniku!";
let len = message.chars().count();
println!("Długość wiadomości: {}", len);

// Output: Długość wiadomości: 19
```

## Głębsze zagadnienia:

Wcześniej w języku Rust istniała funkcja `len()` dla typu `str`. Jednak została ona zastąpiona przez metodę `len()` dla typu `String`, aby uniknąć problemów bezpieczeństwa.

Alternatywą dla wykorzystania metody `chars()` jest wykorzystanie biblioteki `unicode_segmentation`, która pozwala na dokładniejsze dzielenie ciągu znaków.

W implementacji funkcji `len()` wykorzystywany jest wskaźnik do ostatniego bajtu w ciągu, dlatego jest ona wydajniejsza niż wykorzystanie metody `chars()`.

## Zobacz także:

- [Dokumentacja Rust: str::len()](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Dokumentacja Rust: String::len()](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Dokumentacja Rust: std::ops::Index trait](https://doc.rust-lang.org/std/ops/trait.Index.html)
- [Biblioteka unicode_segmentation](https://docs.rs/unicode_segmentation/1.8.0/unicode_segmentation/)